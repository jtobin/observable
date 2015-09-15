{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Observable.Metropolis where

import Control.Comonad
import Control.Comonad.Cofree
import Control.Comonad.Trans.Cofree (runCofreeT, CofreeT)
import qualified Control.Comonad.Trans.Cofree as Cofree
import Control.Monad (replicateM)
import Control.Monad.Free
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Free (runFreeT, FreeT)
import qualified Control.Monad.Trans.Free as Free
import Control.Monad.Trans.State.Strict
import Data.Dynamic
import Data.Functor.Foldable
import Data.Functor.Identity (Identity(..))
import Data.Maybe (fromJust)
import Observable.Core hiding (Parameter)
import Observable.Distribution
import System.Random.MWC.Probability (Prob)
import qualified System.Random.MWC.Probability as Prob
import System.Random.MWC (withSystemRandom, asGenIO, Gen, create)

import Unsafe.Coerce
import Debug.Trace
import Data.Distributive

import System.IO.Unsafe (unsafePerformIO)



printTop :: Show a => Cofree f a -> IO ()
printTop (a :< _) = print a

test :: Model Int
test = beta 1 8 >>= binomial 10

prog :: Conditioned Int
prog = condition 3 test

type Execution a = Cofree ModelF (Node a, Dynamic, Double)

data Chain a = Chain {
    chainScore     :: Double
  , chainExecution :: Execution a
  }

type Transition m a = StateT (Chain a) (Prob m) [Parameter]

metropolis
  :: (Typeable a, PrimMonad m)
  => Int -> Double -> Chain a -> Prob m [[Parameter]]
metropolis n step = evalStateT (replicateM n (transition step))

transition :: Typeable a => PrimMonad m => Double -> Transition m a
transition step = do
  currentState@(Chain currentScore current) <- get
  let proposal = perturbExecution step current

  let proposalScore     = scoreExecution proposal
      currentToProposal = transitionProbability step current proposal
      proposalToCurrent = transitionProbability step proposal current

      ratio = exp . min 0 $
          proposalScore + proposalToCurrent - currentScore - currentToProposal

      acceptanceProbability
        | isNaN ratio = 0
        | otherwise   = ratio

  zc <- lift Prob.uniform

  if   zc < acceptanceProbability
  then put (Chain proposalScore proposal) >> return (collectPositions proposal)
  else return (collectPositions current)

initializeChain
  :: Typeable a
  => Conditioned a
  -> Chain a
initializeChain prog = Chain score initd where
  initd = initializeExecution prog
  score = scoreExecution initd

-- -- | Initialize a Markov chain over conditional program executions.
-- initializeExecution
--   :: forall m a. (PrimMonad m, Typeable a, Show a)
--   => Conditioned a
--   -> Prob m (Execution a)
-- initializeExecution = go where
--   go :: Show a => Conditioned a -> Prob m (Execution a)
--   go term@(Unconditioned :< f) = case f of
--     BetaF a b k -> do
--       z <- Prob.beta a b
--       let dyn = toDyn z
--       rest <- fmap unwrap (go (k z))
--       return $ (extract term, dyn, scoreNode dyn f) :< rest
--
--     BinomialF n p k -> do
--       z <- Prob.binomial n p
--       let dyn = toDyn z
--       rest <- fmap unwrap (go (k z))
--       return $ (extract term, dyn, scoreNode dyn f) :< rest
--
--   go term = return $ goPure term
--
--   goPure t@(Conditioned a :< f) =
--     let dyn = toDyn a
--     in  (extract t, dyn, scoreNode dyn f) :< fmap goPure f
--
--   goPure t@(Closed :< f) = (extract t, toDyn (), 0) :< fmap goPure f
--
--   goPure _ = error "goPure: unexpected unconditioned node"

initializeExecution
  :: Typeable a
  => Conditioned a
  -> Cofree ModelF (Node a, Dynamic, Double)
initializeExecution = extend initialize

initialize
  :: Typeable a
  => Conditioned a
  -> (Node a, Dynamic, Double)
initialize w = (ann, z, scoreNode z etc) where
  (ann, etc) = (extract w, unwrap w)
  z = case ann of
    Unconditioned -> case etc of
      BetaF a b _     -> toDyn (unsafeWithGen $ Prob.sample (Prob.beta a b))
      BinomialF n p _ -> toDyn (unsafeWithGen $ Prob.sample (Prob.binomial n p))
    Conditioned c -> toDyn c
    Closed        -> toDyn ()

perturb
  :: Typeable a
  => Double
  -> Execution a
  -> (Node a, Dynamic, Double)
perturb step w = (ann, z1, scoreNode z1 etc) where
  ((ann, z0, p0), etc) = (extract w, unwrap w)
  u  = unsafeWithGen (Prob.sample (Prob.normal 0 step))
  d  = unsafeWithGen (Prob.sample (Prob.uniformR (-1, 1 :: Int)))
  z1 = case ann of
    Unconditioned -> case etc of
      BetaF a b _     -> toDyn (unsafeFromDyn z0 + u)
      BinomialF n p _ -> toDyn (unsafeFromDyn z0 + u)
    Conditioned c -> toDyn c
    Closed        -> toDyn ()

perturbExecution
  :: Typeable a
  => Double
  -> Execution a
  -> Execution a
perturbExecution step = extend (perturb step)

unsafeWithGen = unsafePerformIO . withSystemRandom . asGenIO


-- | Calculate a probability mass/density for a given distribution and provided
--   parameter.
scoreNode :: Dynamic -> ModelF t -> Double
scoreNode z term = fromJust $ case term of
  BetaF a b _     -> fmap (log . densityBeta a b) (fromDynamic z)
  BinomialF n p _ -> fmap (log . densityBinomial n p) (fromDynamic z)
  ConditionF      -> Just 0

-- | Score the execution of a program.
scoreExecution :: Cofree ModelF (Node a, Dynamic, Double) -> Double
scoreExecution = go where
  go ((_, a, s) :< f) = case f of
    BetaF _ _ k     -> s + go (k (unsafeFromDyn a))
    BinomialF _ _ k -> s + go (k (unsafeFromDyn a))
    ConditionF      -> s

transitionProbability
  :: Double       -- ^ Step size
  -> Execution a  -- ^ Execution at current location
  -> Execution a  -- ^ Execution at proposed location
  -> Double       -- ^ Transition probability
transitionProbability s = go where
  go :: Execution a -> Execution a -> Double
  go ((_, z0, _) :< f) ((_, z1, _) :< _) = case f of
    BetaF _ _ k     ->
      let (u0, u1) = (unsafeFromDyn z0 :: Double, unsafeFromDyn z1 :: Double)
      in  log (densityNormal u0 u1 s) + go (k u0) (k u1)

    BinomialF _ _ k ->
      let (u0, u1) = (unsafeFromDyn z0 :: Int, unsafeFromDyn z1 :: Int)
      in  log (1 / 3) + go (k u0) (k u1)

    ConditionF -> 0

collectPositions :: Execution a -> [Parameter]
collectPositions = go where
  go ((Unconditioned, a, _) :< f) = case f of
    BetaF _ _ k     -> toParameter a : go (k (unsafeFromDyn a))
    BinomialF _ _ k -> toParameter a : go (k (unsafeFromDyn a))
    ConditionF      -> []

  go _ = []

unsafeFromDyn :: Typeable a => Dynamic -> a
unsafeFromDyn = fromJust . fromDynamic

toParameter :: Dynamic -> Parameter
toParameter z
  | dynTypeRep z == typeOf (0 :: Int)    = Parameter (unsafeFromDyn z :: Int)
  | dynTypeRep z == typeOf (0 :: Double) = Parameter (unsafeFromDyn z :: Double)
  | otherwise = error "toParameter: unsupported type"

nodesStatus = go where
  go ((s, a, _) :< f) = case f of
    BetaF _ _ k -> s : go (k (unsafeFromDyn a))
    BinomialF _ _ k -> s : go (k (unsafeFromDyn a))
    ConditionF -> []

testo = go where
  go (ann@(s, a, p) :< f) = case f of
    BetaF _ _ k -> (s, a, p) : go (k (unsafeFromDyn a))
    BinomialF _ _ k -> (s, a, p) : go (k (unsafeFromDyn a))
    ConditionF -> [ann]

data Parameter = forall a. Show a => Parameter a

instance Show Parameter where
  show (Parameter s) = show s

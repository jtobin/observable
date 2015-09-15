{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

module Observable.Metropolis where

import Control.Comonad
import Control.Comonad.Cofree
import Control.Monad (replicateM)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.Trans.Class (lift)
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

metropolis :: PrimMonad m => Int -> Double -> Chain a -> Prob m [[Parameter]]
metropolis n step = evalStateT (replicateM n (transition step))

transition :: PrimMonad m => Double -> Transition m a
transition step = do
  currentState@(Chain currentScore current) <- get
  proposal <- lift (perturbExecution step current)

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
  :: (Typeable a, PrimMonad m)
  => Conditioned a
  -> Prob m (Chain a)
initializeChain prog = do
  initd <- initializeExecution prog
  let score = scoreExecution initd
  return $ Chain score initd

-- | Initialize a Markov chain over conditional program executions.
initializeExecution
  :: (PrimMonad m, Typeable a)
  => Conditioned a
  -> Prob m (Execution a)
initializeExecution = go where
  go term@(Unconditioned :< f) = case f of
    BetaF a b _ -> do
      z <- fmap toDyn (Prob.beta a b)
      return $ term =>> \w ->
        (extract w, z, scoreNode z f)

    BinomialF n p _ -> do
      z <- fmap toDyn (Prob.binomial n p)
      return $ term =>> \w ->
        (extract w, z, scoreNode z f)

  go term@(Conditioned a :< f) = do
      let z = toDyn a
      return $ term =>> \w ->
        (extract w, z, scoreNode z f)

  go w = return $ w =>> const (extract w, toDyn (), 0)

perturbExecution
  :: PrimMonad m
  => Double
  -> Execution a
  -> Prob m (Execution a)
perturbExecution s = go where
  go term@((node@Unconditioned, z, _) :< f) = do
    p <- perturbNode z s
    return $ term =>> const (node, p, scoreNode p f)

  go term@((node@Conditioned {}, z, _) :< f) =
    return $ term =>> const (node, z, scoreNode z f)

  go w@((Closed, _, _) :< _) =
    return $ w =>> const (Closed, toDyn (), 0)

perturbNode
  :: PrimMonad m
  => Dynamic
  -> Double
  -> Prob m Dynamic
perturbNode z s
  | dynTypeRep z == typeOf (0 :: Int) = do
      u <- Prob.uniformR (-1, 1 :: Int)
      return . toDyn $ unsafeFromDyn z + u
  | dynTypeRep z == typeOf (0 :: Double) = do
      u <- Prob.normal 0 s
      return . toDyn $ unsafeFromDyn z + u
  | otherwise = error $
      "perturbNode: unsupported type, " ++ show (dynTypeRep z)

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
  go (s :< f) = case f of
    BetaF _ _ k -> s : go (k 0)
    BinomialF _ _ k -> s : go (k 0)
    ConditionF -> []

data Parameter = forall a. Show a => Parameter a

instance Show Parameter where
  show (Parameter s) = show s

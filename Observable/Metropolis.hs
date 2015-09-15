{-# LANGUAGE NoMonomorphismRestriction #-}

module Observable.Metropolis where

import Control.Comonad
import Control.Comonad.Cofree
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.Trans.State.Strict
import Data.Dynamic
import Data.Functor.Foldable
import Data.Functor.Identity (Identity(..))
import Data.Maybe (fromJust)
import Observable.Core
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

type MarkovChain m a = StateT (Chain a) m [Dynamic]

-- transition = do
--   Chain s e <- get
--   proposal  <- lift perturbExecution


-- | Initialize a Markov chain over conditional program executions.
initializeExecution
  :: (PrimMonad m, Typeable a)
  => Gen (PrimState m)
  -> Conditioned a
  -> m (Execution a)
initializeExecution gen = go where
  go term@(Unconditioned :< f) = case f of
    BetaF a b _ -> do
      z <- fmap toDyn (Prob.sample (Prob.beta a b) gen)
      return $ term =>> \w ->
        (extract w, z, scoreNode z f)

    BinomialF n p _ -> do
      z <- fmap toDyn (Prob.sample (Prob.binomial n p) gen)
      return $ term =>> \w ->
        (extract w, z, scoreNode z f)

  go term@(Conditioned a :< f) = do
      let z = toDyn a
      return $ term =>> \w ->
        (extract w, z, scoreNode z f)

  go w = return $ w =>> const (extract w, toDyn (), 0)

perturbExecution
  :: PrimMonad m
  => Gen (PrimState m)
  -> Double
  -> Execution a
  -> m (Execution a)
perturbExecution gen s = go where
  go term@((node@Unconditioned, z, _) :< f) = do
    p <- perturbNode gen z s
    return $ term =>> const (node, p, scoreNode p f)

  go term@((node@Conditioned {}, z, _) :< f) =
    return $ term =>> const (node, z, scoreNode z f)

  go w@((Closed, _, _) :< _) =
    return $ w =>> const (Closed, toDyn (), 0)

perturbNode
  :: PrimMonad m
  => Gen (PrimState m)
  -> Dynamic
  -> Double
  -> m Dynamic
perturbNode gen z s
  | dynTypeRep z == typeOf (0 :: Int) = do
      u <- Prob.sample (Prob.uniformR (-1, 1 :: Int)) gen
      return . toDyn $ unsafeFromDyn z + u
  | dynTypeRep z == typeOf (0 :: Double) = do
      u <- Prob.sample (Prob.normal 0 s) gen
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

proposalProbability :: Double -> Execution a -> Execution a -> Double
proposalProbability s = go where
  go :: Execution a -> Execution a -> Double
  go ((_, z0, _) :< f) ((_, z1, _) :< _) = case f of
    BetaF _ _ k     ->
      let (u0, u1) = (unsafeFromDyn z0 :: Double, unsafeFromDyn z1 :: Double)
      in  log (densityNormal u0 u1 s) + go (k u0) (k u1)

    BinomialF _ _ k ->
      let (u0, u1) = (unsafeFromDyn z0 :: Int, unsafeFromDyn z1 :: Int)
      in  log (1 / 3) + go (k u0) (k u1)

    ConditionF -> 0

unsafeFromDyn :: Typeable a => Dynamic -> a
unsafeFromDyn = fromJust . fromDynamic


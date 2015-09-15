
module Observable.Metropolis where

import Control.Comonad
import Control.Comonad.Cofree
import Control.Monad.Trans.State.Strict
import Data.Dynamic
import Data.Functor.Foldable
import Data.Functor.Identity (Identity(..))
import Data.Maybe (fromJust)
import Observable.Core
import Observable.Distribution
import System.Random.MWC.Probability (Prob)
import qualified System.Random.MWC.Probability as Prob
import System.Random.MWC (withSystemRandom, asGenIO)
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

type MarkovChain a = StateT (Chain a) Identity [Dynamic]

-- | Initialize a Markov chain over conditional program executions.
initializeExecution :: Typeable a => Conditioned a -> Execution a
initializeExecution = go where
  go term@(Unconditioned :< f) = case f of
    BetaF a b _ -> term =>> \w ->
      let z = toDyn (unsafeSample (Prob.beta a b))
      in  (extract w, z, scoreNode z f)

    BinomialF n p _ -> term =>> \w ->
      let z = toDyn (unsafeSample (Prob.binomial n p))
      in (extract w, z, scoreNode z f)

    StandardF _ -> term =>> \w ->
      let z = toDyn (unsafeSample Prob.standard)
      in (extract w, z, scoreNode z f)

    NormalF m s _ -> term =>> \w ->
      let z = toDyn (unsafeSample (Prob.normal m s))
      in (extract w, z, scoreNode z f)

  go term@(Conditioned a :< f) = term =>> \w -> let z = toDyn a in
      (extract w, z, scoreNode z f)

  go w = w =>> const (extract w, toDyn (), 0)

perturbExecution :: Double -> Execution a -> Execution a
perturbExecution s = go where
  go term@((node@Unconditioned, z, _) :< f) = term =>> \w ->
    let p = perturbNode z s
    in  (node, p, scoreNode p f)

  go term@((node@Conditioned {}, z, _) :< f) = term =>> const
    (node, z, scoreNode z f)

  go w@((Closed, _, _) :< _) = w =>> const (Closed, toDyn (), 0)


perturbNode :: Dynamic -> Double -> Dynamic
perturbNode z s
  | dynTypeRep z == typeOf (0 :: Int) = toDyn $
      fromJust (fromDynamic z) + unsafeSample (Prob.uniformR (-1, 1 :: Int))
  | dynTypeRep z == typeOf (0 :: Double) = toDyn $
      fromJust (fromDynamic z) + unsafeSample (Prob.normal 0 s)
  | otherwise = error "perturbNode: unsupported type"

-- | Unsafely sample from a distribution.
unsafeSample :: Prob IO a -> a
unsafeSample = unsafePerformIO . withSystemRandom . asGenIO . Prob.sample

-- | Calculate a probability mass/density for a given distribution and provided
--   parameter.
scoreNode :: Dynamic -> ModelF t -> Double
scoreNode z term = fromJust $ case term of
  BetaF a b _     -> fmap (log . densityBeta a b) (fromDynamic z)
  BinomialF n p _ -> fmap (log . densityBinomial n p) (fromDynamic z)
  StandardF _     -> fmap (log . densityNormal 0 1) (fromDynamic z)
  NormalF m s _   -> fmap (log . densityNormal m s) (fromDynamic z)
  ConditionF      -> Just 0

-- | Score the execution of a program.
scoreExecution :: Cofree ModelF (Node a, Dynamic, Double) -> Double
scoreExecution = go where
  go ((_, a, s) :< f) = case f of
    BetaF _ _ k     -> s + go (k (fromJust $ fromDynamic a))
    BinomialF _ _ k -> s + go (k (fromJust $ fromDynamic a))
    StandardF k     -> s + go (k (fromJust $ fromDynamic a))
    NormalF _ _ k   -> s + go (k (fromJust $ fromDynamic a))
    ConditionF      -> s



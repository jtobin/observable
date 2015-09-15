{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ExistentialQuantification #-}

module Observable.Metropolis where

import Control.Comonad
import Control.Comonad.Cofree
import Control.Monad (replicateM)
import Control.Monad.Primitive (PrimMonad)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict
import Data.Dynamic
import Data.Maybe (fromJust)
import GHC.Prim (RealWorld)
import Observable.Core hiding (Parameter)
import Observable.Distribution
import System.IO.Unsafe (unsafePerformIO)
import System.Random.MWC.Probability (Prob)
import qualified System.Random.MWC.Probability as Prob

-- | An execution of a program.
type Execution a = Cofree ModelF (Node a, Dynamic, Double)

-- | A transition operator between executions.
type Transition m a = StateT (Chain a) (Prob m) [Parameter]

-- | State of a Markov chain over executions.
data Chain a = Chain {
    chainScore     :: Double
  , chainExecution :: Execution a
  }

-- | Initialize a Markov chain over executions.
initializeChain
  :: Typeable a
  => Conditioned a
  -> Chain a
initializeChain prog = Chain score initd where
  initd = execute prog
  score = scoreExecution initd

-- | The Metropolis algorithm.
metropolis
  :: (Typeable a, PrimMonad m)
  => Int -> Double -> Chain a -> Prob m [[Parameter]]
metropolis n step = evalStateT (replicateM n (transition step))

-- | A Metropolis transition.
transition :: Typeable a => PrimMonad m => Double -> Transition m a
transition step = do
  Chain currentScore current <- get
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

-- | Execute a program.
execute
  :: Typeable a
  => Conditioned a
  -> Cofree ModelF (Node a, Dynamic, Double)
execute = extend initialize

-- | Return execution information from the root node of a conditioned AST.
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

-- | Perturb the execution of a program's root node and record the perturbed
--   execution information.
perturb
  :: Typeable a
  => Double
  -> Execution a
  -> (Node a, Dynamic, Double)
perturb step w = (ann, z1, scoreNode z1 etc) where
  ((ann, z0, _), etc) = (extract w, unwrap w)
  u  = unsafeWithGen (Prob.sample (Prob.normal 0 step))
  d  = unsafeWithGen (Prob.sample (Prob.uniformR (-1, 1 :: Int)))
  z1 = case ann of
    Unconditioned -> case etc of
      BetaF {}        -> toDyn (unsafeFromDyn z0 + u)
      BinomialF {}    -> toDyn (unsafeFromDyn z0 + d)
    Conditioned c -> toDyn c
    Closed        -> toDyn ()

-- | Perturb a program's execution and return the perturbed execution.
perturbExecution
  :: Typeable a
  => Double
  -> Execution a
  -> Execution a
perturbExecution step = extend (perturb step)

-- | Calculate a log probability mass/density for a given distribution and
--   observation.
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

-- | Calculate a probability of transitioning between two executions.
transitionProbability
  :: Double       -- ^ Step size
  -> Execution a  -- ^ Execution at current execution
  -> Execution a  -- ^ Execution at proposed execution
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

-- | Collect the execution trace of a program as a list.
collectPositions :: Execution a -> [Parameter]
collectPositions = go where
  go ((Unconditioned, a, _) :< f) = case f of
    BetaF _ _ k     -> toParameter a : go (k (unsafeFromDyn a))
    BinomialF _ _ k -> toParameter a : go (k (unsafeFromDyn a))
    ConditionF      -> []

  go _ = []

-- | Safely coerce a dynamic value to a showable parameter.
toParameter :: Dynamic -> Parameter
toParameter z
  | dynTypeRep z == typeOf (0 :: Int)    = Parameter (unsafeFromDyn z :: Int)
  | dynTypeRep z == typeOf (0 :: Double) = Parameter (unsafeFromDyn z :: Double)
  | otherwise = error "toParameter: unsupported type"

-- | A showable parameter.
data Parameter = forall a. Show a => Parameter a

instance Show Parameter where
  show (Parameter s) = show s

-- | Unsafely coerce a dynamic value to some type.
unsafeFromDyn :: Typeable a => Dynamic -> a
unsafeFromDyn = fromJust . fromDynamic

-- | Unsafely use the system's PRNG for randomness.
unsafeWithGen :: (Prob.Gen RealWorld -> IO a) -> a
unsafeWithGen = unsafePerformIO . Prob.withSystemRandom . Prob.asGenIO


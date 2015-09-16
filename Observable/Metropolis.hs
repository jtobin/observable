{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ExistentialQuantification #-}

module Observable.Metropolis where

import Control.Comonad
import Control.Comonad.Cofree
import Control.Monad (forever)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.Trans.State.Strict
import Data.Dynamic
import Data.Maybe (fromJust)
import GHC.Prim (RealWorld)
import Observable.Core hiding (Parameter)
import Observable.Distribution
import System.IO.Unsafe (unsafePerformIO)
import qualified System.Random.MWC.Probability as P
import qualified System.Random.MWC as MWC

import Pipes
import qualified Pipes.Prelude as Pipes

-- | An execution of a program.
type Execution a = Cofree ModelF (Node a, Dynamic, Double)

-- | A transition operator between executions.
type Transition m a = StateT (Chain a) m [Parameter]

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

-- | Run the Metropolis algorithm and print positions to stdout.
metropolis
  :: Typeable a
  => Int
  -> Double
  -> P.Gen RealWorld
  -> Conditioned a
  -> IO ()
metropolis n step gen model = runEffect $
      generate step gen model
  >-> Pipes.take n
  >-> display

display :: Show a => Consumer a IO r
display = Pipes.mapM_ print

-- | Perform a Metropolis transition and yield its result downstream.
generate
  :: (PrimMonad m, Typeable a)
  => Double
  -> P.Gen (PrimState m)
  -> Conditioned a
  -> Producer [Parameter] m ()
generate step gen model = flip evalStateT initd . forever $ do
    proposal <- transition step gen
    lift (yield proposal)
  where
    initd = initializeChain model

-- | A Metropolis transition.
transition
  :: (PrimMonad m, Typeable a)
  => Double
  -> P.Gen (PrimState m)
  -> Transition (Producer [Parameter] m) a
  -- Transition m a ~ StateT (Chain a) (Producer [Parameter] IO) [Parameter]
transition step gen = do
  Chain currentScore current <- get
  let proposal = perturbExecution step current
      proposalScore     = scoreExecution proposal
      currentToProposal = transitionProbability step current proposal
      proposalToCurrent = transitionProbability step proposal current

      ratio = exp . min 0 $
          proposalScore + proposalToCurrent - currentScore - currentToProposal

      acceptanceProbability
        | isNaN ratio = 0
        | otherwise   = ratio

  zc <- lift . lift $ MWC.uniform gen

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
      BetaF a b _               -> toDyn (unsafeGen $ P.sample (P.beta a b))
      BinomialF n p _           -> toDyn (unsafeGen $ P.sample (P.binomial n p))
      StandardF _               -> toDyn (unsafeGen $ P.sample P.standard)
      NormalF m s _             -> toDyn (unsafeGen $ P.sample (P.normal m s))
      StudentF a b _            -> toDyn (unsafeGen $ P.sample (P.t a 1 b))
      GammaF a b _              -> toDyn (unsafeGen $ P.sample (P.gamma a b))
      InvGammaF a b _           -> toDyn (unsafeGen $ P.sample (P.inverseGamma a b))
      UniformF a b _            -> toDyn (unsafeGen $ P.sample (P.uniformR (a, b)))
      DirichletF vs _           -> toDyn (unsafeGen $ P.sample (P.dirichlet vs))
      SymmetricDirichletF n a _ -> toDyn (unsafeGen $ P.sample (P.symmetricDirichlet n a))
      CategoricalF vs _         -> toDyn (unsafeGen $ P.sample (P.categorical vs))
      DiscreteUniformF n _      -> toDyn (unsafeGen $ P.sample (P.discreteUniform [1..n]))
      IsoGaussF ms s _          -> toDyn (unsafeGen $ P.sample (P.isoGauss ms s))
      PoissonF l _              -> toDyn (unsafeGen $ P.sample (P.poisson l))
      ExponentialF l _          -> toDyn (unsafeGen $ P.sample (P.exponential l))
      ConditionF                -> error "impossible"
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
  u  = unsafeGen (P.sample (P.normal 0 step))
  d  = unsafeGen (P.sample (P.uniformR (-1, 1 :: Int)))
  z1 = case ann of
    Unconditioned -> case etc of
      BetaF {}        -> toDyn (unsafeFromDyn z0 + u)
      BinomialF {}    -> toDyn (unsafeFromDyn z0 + d)

    -- FIXME tedious
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

  -- FIXME remainder (tedious)
  ConditionF      -> Just 0

-- | Score the execution of a program.
scoreExecution :: Cofree ModelF (Node a, Dynamic, Double) -> Double
scoreExecution = go where
  go ((_, a, s) :< f) = case f of
    BetaF _ _ k     -> s + go (k (unsafeFromDyn a))
    BinomialF _ _ k -> s + go (k (unsafeFromDyn a))

    -- FIXME remainder (tedious)
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

    -- FIXME remainder (tedious)
    _ -> 0

-- | Collect the execution trace of a program as a list.
collectPositions :: Execution a -> [Parameter]
collectPositions = go where
  go ((Unconditioned, a, _) :< f) = case f of
    BetaF _ _ k     -> toParameter a : go (k (unsafeFromDyn a))
    BinomialF _ _ k -> toParameter a : go (k (unsafeFromDyn a))
    ConditionF      -> []

  -- FIXME remainder (tedious)
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
unsafeGen :: (P.Gen RealWorld -> IO a) -> a
unsafeGen = unsafePerformIO . P.withSystemRandom . P.asGenIO


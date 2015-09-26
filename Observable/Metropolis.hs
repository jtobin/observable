{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}

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

import Pipes (Producer, Consumer, (>->), runEffect, yield, lift)
import qualified Pipes.Prelude as Pipes

import Prelude hiding (Foldable)
import Data.Functor.Foldable
import Control.Monad.Free

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
display = Pipes.mapM_ (putStrLn . init . drop 1 . show)

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
transition step gen = do
  Chain currentScore current <- get
  let proposal          = perturbExecution step current
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
  then do
    put (Chain proposalScore proposal)
    return $! collectPositions proposal
  else return $! collectPositions current

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
initialize w = (ann, z1, p1) where
  (ann, etc) = (extract w, unwrap w)
  (z1, p1) = case ann of
    Unconditioned -> case etc of
      BetaF a b _     ->
        let z = toDyn (unsafeGen $ P.sample (P.beta a b))
        in  (z, scoreNode z etc)
      BinomialF n p _ ->
        let z = toDyn (unsafeGen $ P.sample (P.binomial n p))
        in  (z, scoreNode z etc)
      StandardF _     ->
        let z = toDyn (unsafeGen $ P.sample P.standard)
        in  (z, scoreNode z etc)
      NormalF m s _   ->
        let z = toDyn (unsafeGen $ P.sample (P.normal m s))
        in  (z, scoreNode z etc)
      StudentF a b _  ->
        let z = toDyn (unsafeGen $ P.sample (P.t a 1 b))
        in  (z, scoreNode z etc)
      GammaF a b _    ->
        let z = toDyn (unsafeGen $ P.sample (P.gamma a b))
        in  (z, scoreNode z etc)
      InvGammaF a b _ ->
        let z = toDyn (unsafeGen $ P.sample (P.inverseGamma a b))
        in  (z, scoreNode z etc)
      UniformF a b _  ->
        let z = toDyn (unsafeGen $ P.sample (P.uniformR (a, b)))
        in  (z, scoreNode z etc)
      DirichletF vs _ ->
        let z = toDyn (unsafeGen $ P.sample (P.dirichlet vs))
        in  (z, scoreNode z etc)
      SymmetricDirichletF n a _ ->
        let z = toDyn (unsafeGen $ P.sample (P.symmetricDirichlet n a))
        in  (z, scoreNode z etc)
      CategoricalF vs _         ->
        let z = toDyn (unsafeGen $ P.sample (P.categorical vs))
        in  (z, scoreNode z etc)
      DiscreteUniformF n _      ->
        let z = toDyn (unsafeGen $ P.sample (P.discreteUniform [1..n]))
        in  (z, scoreNode z etc)
      IsoGaussF ms s _          ->
        let z = toDyn (unsafeGen $ P.sample (P.isoGauss ms s))
        in  (z, scoreNode z etc)
      PoissonF l _              ->
        let z = toDyn (unsafeGen $ P.sample (P.poisson l))
        in  (z, scoreNode z etc)
      ExponentialF l _          ->
        let z = toDyn (unsafeGen $ P.sample (P.exponential l))
        in  (z, scoreNode z etc)
      ConditionF                -> error "impossible"
    Conditioned cs ->
      (toDyn cs, sum $ map (\z -> scoreNode (toDyn z) etc) cs)
    Closed        -> (toDyn (), 0)

-- | Perturb the execution of a program's root node and record the perturbed
--   execution information.
perturb
  :: Typeable a
  => Double
  -> Execution a
  -> (Node a, Dynamic, Double)
perturb step w = (ann, z1, p1) where
  ((ann, z0, _), etc) = (extract w, unwrap w)
  u  = unsafeGen (P.sample (P.normal 0 step))
  d  = unsafeGen (P.sample (P.uniformR (-1, 1 :: Int)))
  (z1, p1) = case ann of
    Unconditioned -> case etc of
      BetaF {}     -> let z = toDyn (unsafeFromDyn z0 + u) in (z, scoreNode z etc)
      NormalF   {} -> let z = toDyn (unsafeFromDyn z0 + u) in (z, scoreNode z etc)
      UniformF {}  -> let z = toDyn (unsafeFromDyn z0 + u) in (z, scoreNode z etc)
      GammaF   {}  -> let z = toDyn (unsafeFromDyn z0 + u) in (z, scoreNode z etc)

      BinomialF {} -> let z = toDyn (unsafeFromDyn z0 + d) in (z, scoreNode z etc)

    Conditioned cs -> (toDyn cs, sum $ map (\z -> scoreNode (toDyn z) etc) cs)
    Closed         -> (toDyn (), 0)

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
  NormalF   m s _ -> fmap (log . densityNormal m s) (fromDynamic z)
  GammaF a b _    -> fmap (log . densityGamma a b) (fromDynamic z)
  UniformF a b _  -> fmap (log . densityUniform a b) (fromDynamic z)

  -- FIXME remainder (tedious)
  ConditionF      -> Just 0

-- | Score the execution of a program.
scoreExecution :: Cofree ModelF (Node a, Dynamic, Double) -> Double
scoreExecution = go where
  go ((_, a, s) :< f) = case f of
    BetaF _ _ k     -> s + go (k (unsafeFromDyn a))
    BinomialF _ _ k -> s + go (k (unsafeFromDyn a))
    NormalF   _ _ k -> s + go (k (unsafeFromDyn a))
    GammaF    _ _ k -> s + go (k (unsafeFromDyn a))
    UniformF  _ _ k -> s + go (k (unsafeFromDyn a))

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

    NormalF _ _ k     ->
      let (u0, u1) = (unsafeFromDyn z0 :: Double, unsafeFromDyn z1 :: Double)
      in  log (densityNormal u0 u1 s) + go (k u0) (k u1)

    GammaF _ _ k     ->
      let (u0, u1) = (unsafeFromDyn z0 :: Double, unsafeFromDyn z1 :: Double)
      in  log (densityNormal u0 u1 s) + go (k u0) (k u1)

    UniformF _ _ k     ->
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
    GammaF _ _ k    -> toParameter a : go (k (unsafeFromDyn a))
    UniformF  _ _ k -> toParameter a : go (k (unsafeFromDyn a))
    NormalF   _ _ k -> toParameter a : go (k (unsafeFromDyn a))
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


{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Observable.Interpreter where

import Control.Monad.Free (iterM)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Measurable.Core (Measure)
import qualified Measurable.Measures as Measurable
import Observable.Core
import qualified System.Random.MWC.Probability as MWC

-- | A forward-mode sampling interpreter.  Produces a sample from the
--   model's predictive distribution and returns it in a monadic context.
simulate :: PrimMonad m => Model a -> MWC.Gen (PrimState m) -> m a
simulate = MWC.sample . iterM alg where
  alg (BinomialF n p next)           = MWC.binomial n p >>= next
  alg (BetaF a b next)               = MWC.beta a b >>= next
  alg (GammaF a b next)              = MWC.gamma a b >>= next
  alg (InvGammaF a b next)           = MWC.inverseGamma a b >>= next
  alg (StandardF next)               = MWC.standard >>= next
  alg (NormalF a b next)             = MWC.normal a b >>= next
  alg (StudentF m k next)            = MWC.t m 1 k >>= next
  alg (UniformF a b next)            = MWC.uniformR (a, b) >>= next
  alg (DirichletF as next)           = MWC.dirichlet as >>= next
  alg (SymmetricDirichletF n a next) = MWC.symmetricDirichlet n a >>= next
  alg (DiscreteUniformF n next)      = MWC.discreteUniform [0..pred n] >>= next
  alg (CategoricalF ps next)         = MWC.categorical ps >>= next
  alg (IsoGaussF ms v next)          = MWC.isoGauss ms v >>= next
  alg (PoissonF l next)              = MWC.poisson l >>= next
  alg (ExponentialF l next)          = MWC.exponential l >>= next

-- | Forward-mode measure interpreter.  Produces a measure over the model's
--   predictive distribution,
measure :: Model a -> Measure a
measure = iterM alg where
  alg (BinomialF n p next) = Measurable.binomial n p >>= next
  alg (BetaF a b next)     = Measurable.beta a b >>= next
  alg (GammaF a b next)    = Measurable.gamma a b >>= next
  alg (StandardF next)     = Measurable.standard >>= next
  alg (NormalF a b next)   = Measurable.normal a b >>= next
  alg _ = error "measure: distribution not supported"


grabVals
  :: forall a m. (Typeable a, PrimMonad m)
  => MWC.Gen (PrimState m) -> Model a -> StateT [Conditioned Dist] m ()
grabVals gen = eval where
  eval :: PrimMonad m => Model a -> StateT [Conditioned Dist] m ()
  eval (Free (BetaF a b k)) = do
    p <- lift $ MWC.sample (MWC.beta a b) gen
    case fmap toDyn (k p) of
      Pure r -> do
        modify ((:) (C (Beta a b (fromJust $ fromDynamic r))))
      Free f -> do
        modify ((:) (U (Beta a b p)))
        eval (k p)







-- at the crux i want to be able to perturb the 'perturbable' parts of the
-- graph
--
-- so i have a graph of nodes, U or C x
--
-- for every U i perturb its value, feed it forward through the graph as an
-- input to C
--
-- (p0, U (Beta a b))                     (p1, U (Beta a b))
--       \                          ~           \
--     (x, C x (Binomial n p0))               (x, C x (Binomial n p1))
--
-- let p0 = U (Beta a b)
-- in  (x, C x (Binomial n p0))
--
-- make proposals over execution traces
--   but here i don't want to consider them as the program executes, eh.  i
--   just want to look at the nodes and propose global changes.
--
--   some of the inputs will go into the conditioned distribution but that's
--   ok.  i just want to make general proposals over them.
--
--   so: look at program.  i could
--
--   have program, collect unconditioned nodes
--
--
--
-- log (betaDensity a b p0) + log (binomialDensity n p0 x)
--

data Dist =
    Beta Double Double Double Dist
  | Binomial Int Double Int Dist
  | Dirac
  deriving Show

data Conditioned a = C a | U a deriving Show

fromJust (Just x) = x

eval = go where
  go (Beta a b d) =
    let x = eval (Beta a b)
    in








-- import Control.Monad.Free (Free(..))
-- import Control.Monad.Trans.Class (lift)
-- import Control.Monad.Trans.State.Strict (modify, StateT)
-- import Data.Dynamic
--
-- grabVals
--   :: forall a m. (Typeable a, PrimMonad m)
--   => MWC.Gen (PrimState m) -> Model a -> StateT [Conditioned Dist] m ()
-- grabVals gen = eval where
--   eval :: PrimMonad m => Model a -> StateT [Conditioned Dist] m ()
--   eval (Free (BetaF a b k)) = do
--     p <- lift $ MWC.sample (MWC.beta a b) gen
--     case fmap toDyn (k p) of
--       Pure r -> do
--         modify ((:) (C (Beta a b (fromJust $ fromDynamic r))))
--       Free f -> do
--         modify ((:) (U (Beta a b p)))
--         eval (k p)
--
--   eval (Free (BinomialF n p k)) = do
--     x <- lift $ MWC.sample (MWC.binomial n p) gen
--     case fmap toDyn (k x) of
--       Pure r -> do
--         modify ((:) (C (Binomial n p (fromJust $ fromDynamic r))))
--       Free f -> do
--         modify ((:) (U (Binomial n p x)))
--         eval (k x)
--
--   eval (Pure _) = return ()

-- this is not sufficient; i lose the ability to perturb things and have those
-- perturbations propagated appropriately.
--
-- i would be really golden if i could reify this fucking thing as a graph!
--
-- what if i could compile a program to a PHOAS structure?  then reify that
-- appropriately?  much much easier to work with.
--
-- in fact i don't even need to do sampling then.  just work with the graph,
-- perturb it as necessary..







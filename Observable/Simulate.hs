{-# OPTIONS_GHC -Wall #-}

module Observable.Simulate where

import Control.Monad.Free (iterM)
import Control.Monad.Primitive (PrimMonad, PrimState)
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
  alg ConditionF                     = error "impossible (ConditionF)"


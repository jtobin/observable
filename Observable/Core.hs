{-# LANGUAGE DeriveFunctor #-}

module Observable.Core (
    module Data.Sampling.Types
  , ModelF(..)
  , Model

  , Conditioned(..) -- FIXME i may want to keep the constructor private
  , condition

  -- * smart constructors
  , beta
  , binomial
  , standard
  , normal
  , student
  , gamma
  , invGamma
  , uniform
  , dirichlet
  , symmetricDirichlet
  , categorical
  , discreteUniform
  , isoGauss
  , poisson
  , exponential
  ) where

import Control.Monad.Free (Free, liftF)
import Data.Sampling.Types

-- | @Observable@ terms.
data ModelF k =
    BetaF Double Double (Double -> k)
  | BinomialF Int Double (Int -> k)
  | StandardF (Double -> k)
  | NormalF Double Double (Double -> k)
  | StudentF Double Double (Double -> k)
  | GammaF Double Double (Double -> k)
  | InvGammaF Double Double (Double -> k)
  | UniformF Double Double (Double -> k)
  | DirichletF [Double] ([Double] -> k)
  | SymmetricDirichletF Int Double ([Double] -> k)
  | CategoricalF [Double] (Int -> k)
  | DiscreteUniformF Int (Int -> k)
  | IsoGaussF [Double] Double ([Double] -> k)
  | PoissonF Double (Int -> k)
  | ExponentialF Double (Double -> k)
  deriving Functor

-- | An @Observable@ program.
type Model = Free ModelF

beta :: Double -> Double -> Model Double
beta a b = liftF (BetaF a b id)

binomial :: Int -> Double -> Model Int
binomial n p = liftF (BinomialF n p id)

standard :: Model Double
standard = liftF (StandardF id)

normal :: Double -> Double -> Model Double
normal m s = liftF (NormalF m s id)

student :: Double -> Double -> Model Double
student m v = liftF (StudentF m v id)

gamma :: Double -> Double -> Model Double
gamma a b = liftF (GammaF a b id)

invGamma :: Double -> Double -> Model Double
invGamma a b = liftF (InvGammaF a b id)

uniform :: Double -> Double -> Model Double
uniform a b = liftF (UniformF a b id)

dirichlet :: [Double] -> Model [Double]
dirichlet as = liftF (DirichletF as id)

symmetricDirichlet :: Int -> Double -> Model [Double]
symmetricDirichlet n a = liftF (SymmetricDirichletF n a id)

categorical :: [Double] -> Model Int
categorical cs = liftF (CategoricalF cs id)

discreteUniform :: Int -> Model Int
discreteUniform n = liftF (DiscreteUniformF n id)

isoGauss :: [Double] -> Double -> Model [Double]
isoGauss ms v = liftF (IsoGaussF ms v id)

poisson :: Double -> Model Int
poisson l = liftF (PoissonF l id)

exponential :: Double -> Model Double
exponential l = liftF (ExponentialF l id)

-- | A conditioned model is one that returns a constant value.
newtype Conditioned a = Conditioned (Model a)

-- | Condition a model on a value.
condition :: a -> Model a -> Model a -- FIXME? -> Conditioned a
condition x = fmap (const x)


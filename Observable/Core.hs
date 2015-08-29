{-# LANGUAGE DeriveFunctor #-}

module Observable.Core (
    module Data.Sampling.Types
  , ObservableF(..)
  , Observable

  , Conditioned(..) -- FIXME in practice i'll prob want to keep the constructor private
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
data ObservableF k =
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
type Observable = Free ObservableF

beta :: Double -> Double -> Observable Double
beta a b = liftF (BetaF a b id)

binomial :: Int -> Double -> Observable Int
binomial n p = liftF (BinomialF n p id)

standard :: Observable Double
standard = liftF (StandardF id)

normal :: Double -> Double -> Observable Double
normal m s = liftF (NormalF m s id)

student :: Double -> Double -> Observable Double
student m v = liftF (StudentF m v id)

gamma :: Double -> Double -> Observable Double
gamma a b = liftF (GammaF a b id)

invGamma :: Double -> Double -> Observable Double
invGamma a b = liftF (InvGammaF a b id)

uniform :: Double -> Double -> Observable Double
uniform a b = liftF (UniformF a b id)

dirichlet :: [Double] -> Observable [Double]
dirichlet as = liftF (DirichletF as id)

symmetricDirichlet :: Int -> Double -> Observable [Double]
symmetricDirichlet n a = liftF (SymmetricDirichletF n a id)

categorical :: [Double] -> Observable Int
categorical cs = liftF (CategoricalF cs id)

discreteUniform :: Int -> Observable Int
discreteUniform n = liftF (DiscreteUniformF n id)

isoGauss :: [Double] -> Double -> Observable [Double]
isoGauss ms v = liftF (IsoGaussF ms v id)

poisson :: Double -> Observable Int
poisson l = liftF (PoissonF l id)

exponential :: Double -> Observable Double
exponential l = liftF (ExponentialF l id)

-- | A conditioned model is one that returns a constant value.
newtype Conditioned a = Conditioned (Observable a)

-- | Condition
condition :: a -> Observable a -> Conditioned a
condition x = Conditioned . fmap (const x)


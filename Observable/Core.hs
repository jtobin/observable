{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}

module Observable.Core (
    module Data.Sampling.Types

  , ModelF(..) -- FIXME don't want to export ConditionF
  , Model

  , Conditioned
  , Node(..)
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

import Control.Comonad (extract)
import Control.Comonad.Cofree (Cofree(..))
import Control.Monad.Free (Free(..), liftF)
import Data.Functor.Foldable (cata, Fix(..))
import Data.Void (Void, absurd)
import Data.Sampling.Types

-- | @Observable@ terms.
data ModelF k =
    -- * distributional terms
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
  | ConditionF
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

-- | Status of a node.
data Node a = Unconditioned | Conditioned [a] | Closed deriving Show

-- | A Conditioned model is annotated with conditioned/unconditioned status.
type Conditioned a = Cofree ModelF (Node a)

condition :: Model a -> [a] -> Conditioned a
condition model xs = annotate sized where
  fixed  = affix (model >> liftF ConditionF)
  sized  = sizes fixed
  annotate (a :< f) = case a of
    0 -> Closed :< fmap annotate f
    1 -> Conditioned xs :< fmap annotate f
    _ -> Unconditioned :< fmap annotate f

-- | Bottom-up annotation.
synth :: Functor f => (f a -> a) -> Fix f -> Cofree f a
synth f = cata alg where
  alg g = f (fmap extract g) :< g

-- | Convert a Free f to a Fix f.
affix :: Functor f => Free f Void -> Fix f
affix = toFix where
  toFix (Free f) = Fix (fmap toFix f)
  toFix (Pure r) = absurd r

-- | Annotate a fixed model with heights.
sizes :: Fix ModelF -> Cofree ModelF Int
sizes = synth alg where
  alg (BetaF _ _ k)               = succ (k 0)
  alg (BinomialF _ _ k)           = succ (k 0)
  alg (StandardF k)               = succ (k 0)
  alg (NormalF _ _ k)             = succ (k 0)
  alg (StudentF _ _ k)            = succ (k 0)
  alg (GammaF _ _ k)              = succ (k 0)
  alg (InvGammaF _ _ k)           = succ (k 0)
  alg (UniformF _ _ k)            = succ (k 0)
  alg (DirichletF _ k)            = succ (k [])
  alg (SymmetricDirichletF _ _ k) = succ (k [])
  alg (CategoricalF _ k)          = succ (k 0)
  alg (DiscreteUniformF _ k)      = succ (k 0)
  alg (IsoGaussF _ _ k)           = succ (k [])
  alg (PoissonF _ k)              = succ (k 0)
  alg (ExponentialF _ k)          = succ (k 0)
  alg ConditionF                  = 0


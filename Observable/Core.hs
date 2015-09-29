{-# LANGUAGE DeriveFunctor #-}

module Observable.Core where

import Data.Dynamic
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Control.Monad.Free

data ModelF k =
    -- * distributional terms
    BetaF Double Double String (Double -> k)
  | BinomialF Int Double String (Int -> k)
  | StandardF String (Double -> k)
  | NormalF Double Double String (Double -> k)
  | StudentF Double Double String (Double -> k)
  | GammaF Double Double String (Double -> k)
  | InvGammaF Double Double String (Double -> k)
  | UniformF Double Double String (Double -> k)
  | DirichletF [Double] String ([Double] -> k)
  | SymmetricDirichletF Int Double String ([Double] -> k)
  | CategoricalF [Double] String (Int -> k)
  | DiscreteUniformF Int String (Int -> k)
  | IsoGaussF [Double] Double String ([Double] -> k)
  | PoissonF Double String (Int -> k)
  | ExponentialF Double String (Double -> k)
  deriving Functor

type Model = Free ModelF

beta :: Double -> Double -> String -> Model Double
beta a b tag = liftF (BetaF a b tag id)

binomial :: Int -> Double -> String -> Model Int
binomial n p tag = liftF (BinomialF n p tag id)

standard :: String -> Model Double
standard tag = liftF (StandardF tag id)

normal :: Double -> Double -> String -> Model Double
normal m s tag = liftF (NormalF m s tag id)

student :: Double -> Double -> String -> Model Double
student m v tag = liftF (StudentF m v tag id)

gamma :: Double -> Double -> String -> Model Double
gamma a b tag = liftF (GammaF a b tag id)

invGamma :: Double -> Double -> String -> Model Double
invGamma a b tag = liftF (InvGammaF a b tag id)

uniform :: Double -> Double -> String -> Model Double
uniform a b tag = liftF (UniformF a b tag id)

dirichlet :: [Double] -> String -> Model [Double]
dirichlet as tag = liftF (DirichletF as tag id)

symmetricDirichlet :: Int -> Double -> String -> Model [Double]
symmetricDirichlet n a tag = liftF (SymmetricDirichletF n a tag id)

categorical :: [Double] -> String -> Model Int
categorical cs tag = liftF (CategoricalF cs tag id)

discreteUniform :: Int -> String -> Model Int
discreteUniform n tag = liftF (DiscreteUniformF n tag id)

isoGauss :: [Double] -> Double -> String -> Model [Double]
isoGauss ms v tag = liftF (IsoGaussF ms v tag id)

poisson :: Double -> String -> Model Int
poisson l tag = liftF (PoissonF l tag id)

exponential :: Double -> String -> Model Double
exponential l tag = liftF (ExponentialF l tag id)

data Target = Target {
    lTarget  :: Map String Dynamic -> Double
  , glTarget :: Maybe (Map String Dynamic -> Map String Dynamic)
  }

createTargetWithGradient
  :: (Map String Dynamic -> Double)
  -> (Map String Dynamic -> Map String Dynamic)
  -> Target
createTargetWithGradient f g = Target f (Just g)

createTargetWithoutGradient :: (Map String Dynamic -> Double) -> Target
createTargetWithoutGradient f = Target f Nothing

handleGradient :: Maybe t -> t
handleGradient Nothing  = error "handleGradient: target has no gradient"
handleGradient (Just g) = g

observations :: Ord k => [(k, a)] -> Map k a
observations = Map.fromList

parameters :: Ord k => [(k, a)] -> Map k a
parameters = Map.fromList

discrete :: Int -> Dynamic
discrete = toDyn

continuous :: Double -> Dynamic
continuous = toDyn


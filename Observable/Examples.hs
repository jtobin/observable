
module Observable.Examples where

import Data.Traversable
import Observable.Core
import Observable.Interpreter
import Observable.Utils

-- | A simple beta-binomial model for testing.
betaBinomial :: Int -> Double -> Double -> Observable Int
betaBinomial n a b = do
  p <- observe "p" (beta a b)
  observe "x" (binomial n p)

-- | An example beta-binomial model with saturated hyperparameters.
exampleBb :: Observable Int
exampleBb = betaBinomial 10 1 8

-- -- | Example scores for the exampleBb program.
example :: Double
example = logPosterior vs exampleBb where
  vs = parameters [("p", continuous 0.1), ("x", discrete 8)]

-- | An example Bayesian linear regression model.
linearFit :: Double -> Double -> [Double] -> Observable [Double]
linearFit c d xs = do
  a   <- observe "intercept" standard
  b   <- observe "slope" standard
  var <- observe "variance" (gamma c d)
  for xs (\x -> observe "ys" (normal (a + b * x) var))

-- | An example Bayesian sinusoidal regression model.
sinusoidal :: [Double] -> Observable [Double]
sinusoidal obs = do
  a <- observe "intercept" (normal 0 10)
  b <- observe "slope" (normal 0 10)
  v <- observe "variance" (invGamma 1 2)
  let model x = observe "y" (normal (a*cos x + b*sin x) (sqrt v))
  for obs model

-- | The sinusoidal model prior, separated.
prior :: Observable (Double, Double, Double)
prior = do
  a <- observe "intercept" (normal 0 10)
  b <- observe "slope" (normal 0 10)
  v <- observe "variance" (invGamma 1 2)
  return (a, b, v)

-- | THe sinusoidal model likelihood, separated.
likelihood :: [Double] -> (Double, Double, Double) -> Observable [Double]
likelihood obs (a, b, v) = do
  let model x = observe "y" (normal (a*cos x + b*sin x) (sqrt v))
  for obs model

-- | An alternative spec for the sinusoidal model.
sinusoidalModel :: [Double] -> Observable [Double]
sinusoidalModel obs = prior >>= likelihood obs


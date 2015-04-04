
module Observable.Examples where

import Control.Monad
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
example :: Environment Double
example = logPosterior vs exampleBb where
  vs = parameters [("p", double 0.1), ("x", int 8)]

-- | An example Bayesian linear regression model.
linearFit :: Double -> Double -> [Double] -> Observable [Double]
linearFit c d xs = do
  a   <- observe "intercept" standard
  b   <- observe "slope" standard
  var <- observe "variance" (gamma c d)
  for xs (\x -> observe "ys" (normal (a + b * x) var))

-- | An example Bayesian linear regression model.
trigFit :: Double -> Double -> [Double] -> Observable [Double]
trigFit c d xs = do
  as  <- replicateM 3 (observe "coeffs" standard)
  var <- observe "variance" (gamma c d)
  let model v = sum $ zipWith (*) as [1, cos v, sin v]
  for xs (\x -> observe "ys" (normal (model x) var))


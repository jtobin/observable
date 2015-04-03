
module Observable.Examples where

import qualified Data.Map as Map
import Observable.Core
import Observable.Interpreter

-- | A simple beta-binomial model for testing.
betaBinomial :: Int -> Double -> Double -> Observable Int
betaBinomial n a b = do
  p <- observe "p" (Beta a b)
  observe "x" (Binomial n p)

-- | An example beta-binomial model with saturated hyperparameters.
exampleBb :: Observable Int
exampleBb = betaBinomial 10 1 8

-- -- | Example scores for the exampleBb program.
example :: Environment Double
example = logPosterior vs exampleBb where
  vs = Map.fromList [("p", double 0.1), ("x", int 8)]

-- | An example Bayesian linear regression model.
linearFit :: Double -> Double -> [Double] -> Observable [Double]
linearFit c d xs = do
  a   <- observe "intercept" Standard
  b   <- observe "slope" Standard
  var <- observe "variance" (Gamma c d)
  let mus = fmap (\v -> a + b * v) xs
  observe "ys" (IsoGauss mus var)

-- | An example Bayesian linear regression model.
trigFit :: Double -> Double -> [Double] -> Observable [Double]
trigFit c d xs = do
  as  <- observe "coeffs" (IsoStandard 3)
  var <- observe "variance" (Gamma c d)
  let model v = sum $ zipWith (*) as [1, cos v, sin v]
  let mus     = fmap model xs
  observe "ys" (IsoGauss mus var)


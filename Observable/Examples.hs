
module Observable.Examples where

import Data.Map (Map)
import qualified Data.Map as Map
import Observable.Core
import Observable.Interpreter

-- | A simple beta-binomial model for testing.
betaBinomial :: Lit -> Lit -> Lit -> Program Lit
betaBinomial n a b = do
  p <- observe "p" (Beta a b)
  x <- observe "x" (Binomial n p)
  returning x

-- | An example beta-binomial model with saturated hyperparameters.
exampleBb :: Program Lit
exampleBb = betaBinomial 10 1 8

-- | Example scores for the exampleBb program.
example :: Map String Double
example = logPosterior vs exampleBb where
  vs = Map.fromList [("p", 0.1), ("x", 8)]

-- | An example Bayesian linear regression model.
linearFit :: Lit -> Lit -> Lit -> Program Lit
linearFit c d obs = do
  a   <- observe "intercept" (Gaussian 0 1)
  b   <- observe "slope" (Gaussian 0 1)
  var <- observe "variance" (Gamma c d)
  let linear v = a + b * v
  x   <- observe "x" (Gaussian (linear obs) var)
  returning x




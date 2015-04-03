
module Observable.Examples where

import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Traversable
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
linearFit c d (LitList xs) = do
  a   <- observe "intercept" StandardGaussian
  b   <- observe "slope" StandardGaussian
  var <- observe "variance" (Gamma c d)
  let linear v = a + b * v
      mus      = list (fmap linear xs)
  ys  <- observe "ys" (IsoGaussian mus var)
  returning ys

-- | An example 1D Gaussian mixture model, containing three mixtures.
--
--   NB cut this, too much superfluous work and not enough reward
-- mixture n a = do
--   ps <- observe "theta" (SymmetricDirichlet 3 a)
--   LitList zs <- observe "labels" (Multinomial n ps)
--
--   let mean group = case group of
--         LitInt 0 -> double 0
--         LitInt 1 -> double 1
--         LitInt 2 -> double 5
--
--       means = list (fmap mean zs)
--
--   xs <- observe "xs" (IsoGaussian means 1)
--   returning xs


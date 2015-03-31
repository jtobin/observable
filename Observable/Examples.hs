
module Observable.Examples where

import Observable.Core

-- | A simple beta-binomial model for testing.
betaBinomial :: Lit -> Lit -> Lit -> Program Lit
betaBinomial n a b = do
  p <- observe "p" (Beta a b)
  x <- observe "x" (Binomial n p)
  returning x


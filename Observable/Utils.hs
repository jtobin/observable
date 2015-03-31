
module Observable.Utils where

import Control.Monad
import Control.Monad.Primitive
import System.Random.MWC

-- | A sampling function for the binomial distribution.
binomial
  :: (Variate a, PrimMonad m, Fractional a, Ord a)
  => Int
  -> a
  -> Gen (PrimState m)
  -> m Int
binomial n p g = do
  vals <- replicateM n (uniform g)
  return . length $ filter (< p) vals


{-# LANGUAGE NoMonomorphismRestriction #-}

module Observable.Utils where

import Control.Monad
import Control.Monad.Primitive
import Data.Function
import Data.List
import System.Random.MWC
import System.Random.MWC.Distributions

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

-- | A sampling function for the multinomial distribution.
multinomial
  :: (Variate a, PrimMonad m, Ord a, Num a)
  => Int
  -> [a]
  -> Gen (PrimState m)
  -> m [Int]
multinomial n (p:ps) g = do
  let cumulative = scanl (+) p ps
  replicateM n $ do
    z <- uniform g
    let Just group = findIndex (> z) cumulative
    return group

-- | A sampling function for the symmetric Dirichlet distribution.
symmetricDirichlet
  :: PrimMonad m
  => Int
  -> Double
  -> Gen (PrimState m)
  -> m [Double]
symmetricDirichlet n a g = do
  zs <- replicateM n (gamma a 1 g)
  return $ fmap (/ sum zs) zs


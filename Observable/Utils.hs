{-# LANGUAGE NoMonomorphismRestriction #-}

module Observable.Utils where

import Control.Monad
import Control.Monad.Primitive
import Data.Function
import Data.List
import Data.Monoid
import Data.Map (Map)
import qualified Data.Map as Map
import Observable.Core
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

extractInt :: String -> Environment Lit -> Int
extractInt name store = case Map.lookup name store of
  Nothing -> error $ "parameter '" <> name <> "' not found"
  Just v  -> case v of
    LitInt j -> j
    _ -> error $
      "expected Integer for parameter '" <> name <> "', got: " <> show v

extractDouble :: String -> Environment Lit -> Double
extractDouble name store = case Map.lookup name store of
  Nothing -> error $ "parameter '" <> name <> "' not found"
  Just v  -> case v of
    LitDouble j -> j
    _ -> error $
      "expected Double for parameter '" <> name <> "', got: " <> show v

extractVec :: String -> Environment Lit -> [Lit]
extractVec name store = case Map.lookup name store of
  Nothing -> error $ "parameter '" <> name <> "' not found"
  Just v  -> case v of
    LitVec j -> j
    _ -> error $
      "expected Vector for parameter '" <> name <> "', got: " <> show v

grabDouble :: String -> Lit -> Double
grabDouble name (LitDouble j) = j
grabDouble name _ = error $ "expected Double for value inside '" <> name <> "'"


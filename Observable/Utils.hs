{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE GADTs #-}

module Observable.Utils where

import Data.Dynamic
import Data.Monoid
import qualified Data.Map as Map
import Observable.Core
import Numeric.SpecFunctions

extractInt :: String -> Parameters -> Int
extractInt name store = case Map.lookup name store of
  Nothing -> error $ "parameter '" <> name <> "' not found"
  Just v  -> case v of
    Discrete j -> j
    _ -> error $
      "expected discrete parameter " <> name <> "', got: " <> show v

extractDouble :: String -> Parameters -> Double
extractDouble name store = case Map.lookup name store of
  Nothing -> error $ "parameter '" <> name <> "' not found"
  Just v  -> case v of
    Continuous j -> j
    _ -> error $
      "expected continuous parameter '" <> name <> "', got: " <> show v

extractVec :: String -> Parameters -> [Double]
extractVec name store = case Map.lookup name store of
  Nothing -> error $ "parameter '" <> name <> "' not found"
  Just v  -> case v of
    Vector vec -> vec
    _ -> error $
      "expected vector parameter '" <> name <> "', got: " <> show v

grabDouble :: String -> Dynamic -> Double
grabDouble name v = case fromDynamic v of
  Just j  -> j
  Nothing -> error $ "expected Double for value inside '" <> name <> "'"

-- | Inverse gamma density.
invGammaDensity :: Double -> Double -> Double -> Double
invGammaDensity a b x =
  b ** a / exp (logGamma a) * x ** (negate a - 1) * exp (negate b / x)

-- | Nonstandard t density.
tDensity :: Double -> Double -> Double -> Double -> Double
tDensity m s v x =
    kfac * (1 + recip v * (x - m) ^ 2 / s) ** (negate (v + 1) / 2)
  where
    kfac = exp $
        logGamma ((v + 1) / 2)
      - logGamma (v / 2)
      - 0.5 * (log pi + log v + log s)

-- | Uniform density function.
uniformDensity :: Double -> Double -> Double -> Double
uniformDensity a b x
  | x < a || x > b = 0
  | otherwise      = 1 / (b - a)

-- | Alias for Map.fromList.
parameters :: [(String, Parameter)] -> Parameters
parameters = Map.fromList

-- | Feed a continuation a value at the appropriate type it's expecting.
squash :: (a -> r) -> Distribution a -> r
squash f Beta {}     = f 0
squash f Binomial {} = f 0
squash f Standard    = f 0
squash f Normal {}   = f 0
squash f Student {}  = f 0
squash f Gamma {}    = f 0
squash f InvGamma {} = f 0
squash f Uniform {}  = f 0

-- | Add a value to a Maybe, treating Nothing as zero.
add :: Num a => a -> Maybe a -> Maybe a
add v Nothing    = Just v
add v0 (Just v1) = Just (v0 + v1)


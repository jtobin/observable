{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Observable.Utils where

import Data.Dynamic
import Data.Monoid
import qualified Data.Map as Map
import Observable.Core
import Numeric.SpecFunctions

extractInt :: String -> Environment Dynamic -> Int
extractInt name store = case Map.lookup name store of
  Nothing -> error $ "parameter '" <> name <> "' not found"
  Just v  -> case fromDynamic v of
    Just j -> j
    _      -> error $
      "expected Integer for parameter '" <> name <> "', got: " <> show v

extractDouble :: String -> Environment Dynamic -> Double
extractDouble name store = case Map.lookup name store of
  Nothing -> error $ "parameter '" <> name <> "' not found"
  Just v  -> case fromDynamic v of
    Just j -> j
    _      -> error $
      "expected Double for parameter '" <> name <> "', got: " <> show v

extractVec :: (Num a, Typeable a) => String -> Environment Dynamic -> [a]
extractVec name store = case Map.lookup name store of
  Nothing -> error $ "parameter '" <> name <> "' not found"
  Just v  -> case fromDynamic v of
    Just j -> j
    _ -> error $
      "expected Vector for parameter '" <> name <> "', got: " <> show v

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


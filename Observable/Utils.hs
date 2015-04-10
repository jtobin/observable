{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE GADTs #-}

module Observable.Utils where

import Data.Dynamic
import Data.Monoid
import qualified Data.Map as Map
import Observable.Core
import Numeric.SpecFunctions
import Statistics.Distribution hiding (Distribution)
import qualified Statistics.Distribution.Beta as Statistics
import qualified Statistics.Distribution.Binomial as Statistics
import qualified Statistics.Distribution.Gamma as Statistics
import qualified Statistics.Distribution.Normal as Statistics
import qualified Statistics.Distribution.Uniform as Statistics

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

-- | Score error helper.
scoreError :: (Show a, Show b) => String -> b -> a -> e
scoreError ptype dist value = error $
    "expected "
  <> ptype
  <> " value while evaluating density for '"
  <> show dist
  <> "'; received '"
  <> show value
  <> "'"

-- | Calculate a probability mass/density for a given distribution and provided
--   parameter.
score :: Parameter -> Distribution a -> (Dynamic, Double)
score val dist@(Binomial n p) = case val of
  Discrete j ->
    let paramScore = log $ probability (Statistics.binomial n p) j
    in (toDyn j, paramScore)
  v -> scoreError "discrete" dist v

score val dist@(Beta a b) = case val of
  Continuous x ->
    let paramScore = log $ density (Statistics.betaDistr a b) x
    in (toDyn x, paramScore)
  v -> scoreError "continuous" dist v

score val dist@(Gamma a b) = case val of
  Continuous x ->
    let paramScore = log $ density (Statistics.gammaDistr a b) x
    in  (toDyn x, paramScore)
  v -> scoreError "continuous" dist v

score val dist@(InvGamma a b) = case val of
  Continuous x ->
    let paramScore = log $ invGammaDensity a b x
    in  (toDyn x, paramScore)
  v -> scoreError "continuous" dist v

score val dist@(Normal a b) = case val of
  Continuous x ->
    let paramScore = log $ density (Statistics.normalDistr a b) x
    in  (toDyn x, paramScore)
  v -> scoreError "continuous" dist v

score val dist@(Student a b) = case val of
  Continuous x ->
    let paramScore = log $ tDensity a 1 b x
    in  (toDyn x, paramScore)
  v -> scoreError "continuous" dist v

score val dist@Standard = case val of
  Continuous x ->
    let paramScore = log $ density Statistics.standard x
    in  (toDyn x, paramScore)
  v -> scoreError "continuous" dist v

score val dist@(Uniform a b) = case val of
  Continuous x ->
    let paramScore = log $ density (Statistics.uniformDistr a b) x
    in  (toDyn x, paramScore)
  v -> scoreError "continuous" dist v


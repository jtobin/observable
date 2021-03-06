{-# LANGUAGE GADTs #-}

module Observable.Utils where

-- import Data.Dynamic (Dynamic, toDyn)
-- import Data.Map (Map)
-- import qualified Data.Map as Map (fromList)
-- import Data.Monoid ((<>))
-- import Observable.Distribution

import Control.Comonad (extract, (=>>))
import Control.Comonad.Cofree (Cofree(..))
import Control.Monad.Free (Free(..))
import Data.Functor.Foldable (cata, Fix(..))
import Data.Void (Void, absurd)
import Observable.Core

-- type Environment a = Map String a
--
-- -- | Alias for Map.fromList.
-- parameters :: [(String, Parameter)] -> Parameters
-- parameters = Map.fromList
--
-- -- | Alias for Map.fromList.
-- observations :: [(String, Parameter)] -> Observations
-- observations = Map.fromList
--
-- -- | Add a value to a Maybe, treating Nothing as zero.
-- add :: Num a => a -> Maybe a -> Maybe a
-- add v Nothing    = Just v
-- add v0 (Just v1) = Just (v0 + v1)
--
-- -- | Score error helper.
-- scoreError :: (Show a, Show b) => String -> b -> a -> e
-- scoreError ptype dist value = error $
--     "expected "
--   <> ptype
--   <> " value while evaluating density for '"
--   <> show dist
--   <> "'; received '"
--   <> show value
--   <> "'"

-- | Calculate a probability mass/density for a given distribution and provided
--   parameter.
-- score :: Parameter -> Distribution a -> (Dynamic, Double)
-- score val dist@(Binomial n p) = case val of
--   Discrete j ->
--     let paramScore = log $ densityBinomial n p j
--     in (toDyn j, paramScore)
--   v -> scoreError "discrete" dist v
--
-- score val dist@(Beta a b) = case val of
--   Continuous x ->
--     let paramScore = log $ densityBeta a b x
--     in (toDyn x, paramScore)
--   v -> scoreError "continuous" dist v
--
-- score val dist@(Gamma a b) = case val of
--   Continuous x ->
--     let paramScore = log $ densityGamma a b x
--     in  (toDyn x, paramScore)
--   v -> scoreError "continuous" dist v
--
-- score val dist@(InvGamma a b) = case val of
--   Continuous x ->
--     let paramScore = log $ densityInvGamma a b x
--     in  (toDyn x, paramScore)
--   v -> scoreError "continuous" dist v
--
-- score val dist@(Normal a b) = case val of
--   Continuous x ->
--     let paramScore = log $ densityNormal a b x
--     in  (toDyn x, paramScore)
--   v -> scoreError "continuous" dist v
--
-- score val dist@(Student a b) = case val of
--   Continuous x ->
--     let paramScore = log $ densityStudent a 1 b x
--     in  (toDyn x, paramScore)
--   v -> scoreError "continuous" dist v
--
-- score val dist@Standard = case val of
--   Continuous x ->
--     let paramScore = log $ densityStandard x
--     in  (toDyn x, paramScore)
--   v -> scoreError "continuous" dist v
--
-- score val dist@(Uniform a b) = case val of
--   Continuous x ->
--     let paramScore = log $ densityUniform a b x
--     in  (toDyn x, paramScore)
--   v -> scoreError "continuous" dist v
--
-- score val dist@(Dirichlet as) = case val of
--   ContinuousVector xs ->
--     let paramScore = log $ densityDirichlet as xs
--     in  (toDyn xs, paramScore)
--   v -> scoreError "vector" dist v
--
-- score val dist@(SymmetricDirichlet _ a) = case val of
--   ContinuousVector xs ->
--     let paramScore = log $ densitySymmetricDirichlet a xs
--     in  (toDyn xs, paramScore)
--   v -> scoreError "vector" dist v
--
-- score val dist@(Categorical ps) = case val of
--   Discrete x ->
--     let paramScore = log $ densityCategorical ps x
--     in  (toDyn x, paramScore)
--   v -> scoreError "discrete" dist v
--
-- score val dist@(DiscreteUniform n) = case val of
--   Discrete x ->
--     let paramScore = log $ densityDiscreteUniform n
--     in  (toDyn x, paramScore)
--   v -> scoreError "discrete" dist v
--
-- score val dist@(IsoGauss ms sd) = case val of
--   ContinuousVector xs ->
--     let paramScore = log $ densityIsoGauss ms sd xs
--     in  (toDyn xs, paramScore)
--   v -> scoreError "vector" dist v
--
-- score val dist@(Poisson l) = case val of
--   Discrete x ->
--     let paramScore = log $ densityPoisson l x
--     in  (toDyn x, paramScore)
--   v -> scoreError "discrete" dist v
--
-- score val dist@(Exponential l) = case val of
--   Continuous x ->
--     let paramScore = log $ densityExponential l x
--     in  (toDyn x, paramScore)
--   v -> scoreError "continuous" dist v


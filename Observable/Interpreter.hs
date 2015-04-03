{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTs #-}

module Observable.Interpreter where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict
import Control.Monad.Primitive
import Data.Functor.Identity
import qualified Data.Map as Map
import Data.Monoid
import Data.Traversable
import Measurable.Core
import qualified Measurable.Measures as Measurable
import Observable.Core
import Observable.Utils
import System.Random.MWC.Probability
import Statistics.Distribution
import qualified Statistics.Distribution.Beta as Statistics
import qualified Statistics.Distribution.Binomial as Statistics
import qualified Statistics.Distribution.Gamma as Statistics
import qualified Statistics.Distribution.Normal as Statistics

-- | A forward-mode sampling interpreter.  Produces a sample from the joint
--   distribution and returns it in IO.
simulate
  :: (Applicative m, PrimMonad m)
  => Observable a
  -> Gen (PrimState m)
  -> m a
simulate expr = sample (eval expr) where
  eval :: (Applicative m, PrimMonad m) => Observable a -> Prob m a
  eval (Pure r) = return r
  eval (Free e) = case e of

    Observe _ dist next -> case dist of
      Binomial n p -> do
        value <- binomial n p
        eval (next value)

      Beta a b -> do
        value <- beta a b
        eval (next value)

      Gamma a b -> do
        value <- gamma a b
        eval (next value)

      InvGamma a b -> do
        value <- inverseGamma a b
        eval (next value)

      Standard -> do
        value <- standard
        eval (next value)

      Normal a b -> do
        value <- normal a b
        eval (next value)

      Student m k -> do
        value <- t m 1 k
        eval (next value)

      IsoGauss mus s -> do
        value <- traverse (`normal` s) mus
        eval (next value)

      IsoStandard n -> do
        value <- replicateM n standard
        eval (next value)

-- | A log posterior score interpreter.  Returns values proportional to the
--   log-posterior probabilities associated with each parameter and
--   observation.
logPosterior :: Environment Lit -> Observable a -> Environment Double
logPosterior ps =
      runIdentity
    . flip runReaderT ps
    . flip execStateT mempty
    . resolve
  where
    resolve
      :: Observable a
      -> StateT (Environment Double) (ReaderT (Environment Lit) Identity) a
    resolve (Pure a) = return a
    resolve (Free e) = case e of
      Observe name dist next -> case dist of

        Binomial n p -> do
          val <- fmap (extractInt name) (lift ask)
          let score = log $ probability (Statistics.binomial n p) val
          modify $ Map.insert name score
          resolve (next val)

        Beta a b -> do
          val <- fmap (extractDouble name) (lift ask)
          let score = log $ density (Statistics.betaDistr a b) val
          modify $ Map.insert name score
          resolve (next val)

        Gamma a b -> do
          val <- fmap (extractDouble name) (lift ask)
          let score = log $ density (Statistics.gammaDistr a b) val
          modify $ Map.insert name score
          resolve (next val)

        InvGamma a b -> do
          val <- fmap (extractDouble name) (lift ask)
          let score = log $ invGamma a b val
          modify $ Map.insert name score
          resolve (next val)

        Normal a b -> do
          val <- fmap (extractDouble name) (lift ask)
          let score = log $ density (Statistics.normalDistr a b) val
          modify $ Map.insert name score
          resolve (next val)

        Student m k -> do
          val <- fmap (extractDouble name) (lift ask)
          let score = log $ tDensity m 1 k val
          modify $ Map.insert name score
          resolve (next val)

        Standard -> do
          val <- fmap (extractDouble name) (lift ask)
          let score = log $ density Statistics.standard val
          modify $ Map.insert name score
          resolve (next val)

        IsoGauss mus s -> do
          val <- fmap (extractVec name) (lift ask)
          let vals = fmap (grabDouble name) val
              scorer m v = log $ density (Statistics.normalDistr m s) v
              score      = sum $ zipWith scorer mus vals
          modify $ Map.insert name score
          resolve (next vals)

        IsoStandard _ -> do
          val <- fmap (extractVec name) (lift ask)
          let vals = fmap (grabDouble name) val
              scorer = log . density Statistics.standard
              score  = sum $ fmap scorer vals
          modify $ Map.insert name score
          resolve (next vals)

-- | Forward-mode measure interpreter.  Produces a measure according to the
--   joint distribution, but only returns the leaf node of the graph.
forwardMeasure :: Observable a -> Measure a
forwardMeasure = measure where
  measure :: Observable a -> Measure a
  measure (Pure r) = return r
  measure (Free e) = case e of
    Observe _ dist next -> case dist of

      Binomial n p -> do
        value <- Measurable.binomial n p
        measure (next value)

      Beta a b -> do
        value <- Measurable.beta a b
        measure (next value)

      Gamma a b -> do
        value <- Measurable.gamma a b
        measure (next value)

      InvGamma a b -> do
        value <- fromDensityFunction (invGamma a b)
        measure (next value)

      Standard -> do
        value <- Measurable.standard
        measure (next value)

      Normal a b -> do
        value <- Measurable.normal a b
        measure (next value)

      Student m k -> do
        value <- fromDensityFunction (tDensity m 1 k)
        measure (next value)

      IsoGauss mus s -> do
        value <- traverse (`Measurable.normal` s) mus
        measure (next value)

      IsoStandard n -> do
        value <- replicateM n Measurable.standard
        measure (next value)


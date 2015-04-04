{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTs #-}

module Observable.Interpreter where

import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict
import Control.Monad.Primitive
import Data.Functor.Identity
import qualified Data.Map as Map
import Data.Monoid
import Measurable.Core
import qualified Measurable.Measures as Measurable
import Observable.Core hiding (
    beta
  , binomial
  , gamma
  , invGamma
  , normal
  , standard
  , student
  )
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
      Binomial n p   -> eval . next =<< binomial n p
      Beta a b       -> eval . next =<< beta a b
      Gamma a b      -> eval . next =<< gamma a b
      InvGamma a b   -> eval . next =<< inverseGamma a b
      Standard       -> eval . next =<< standard
      Normal a b     -> eval . next =<< normal a b
      Student m k    -> eval . next =<< t m 1 k

-- | Forward-mode measure interpreter.  Produces a measure according to the
--   joint distribution, but only returns the leaf node of the graph.
forwardMeasure :: Observable a -> Measure a
forwardMeasure = measure where
  measure :: Observable a -> Measure a
  measure (Pure r) = return r
  measure (Free e) = case e of
    Observe _ dist next -> let continue = measure . next in case dist of
      Binomial n p   -> continue =<< Measurable.binomial n p
      Beta a b       -> continue =<< Measurable.beta a b
      Gamma a b      -> continue =<< Measurable.gamma a b
      InvGamma a b   -> continue =<< fromDensityFunction (invGammaDensity a b)
      Standard       -> continue =<< Measurable.standard
      Normal a b     -> continue =<< Measurable.normal a b
      Student m k    -> continue =<< fromDensityFunction (tDensity m 1 k)

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
          let score = log $ invGammaDensity a b val
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


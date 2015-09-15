
module Observable.Measure where

import Control.Monad.Free (iterM)
import Measurable.Core (Measure)
import qualified Measurable.Measures as Measurable
import Observable.Core

-- | Forward-mode measure interpreter.  Produces a measure over the model's
--   predictive distribution,
measure :: Model a -> Measure a
measure = iterM alg where
  alg (BinomialF n p next) = Measurable.binomial n p >>= next
  alg (BetaF a b next)     = Measurable.beta a b >>= next
  alg (GammaF a b next)    = Measurable.gamma a b >>= next
  alg (StandardF next)     = Measurable.standard >>= next
  alg (NormalF a b next)   = Measurable.normal a b >>= next
  alg _ = error "measure: distribution not supported"


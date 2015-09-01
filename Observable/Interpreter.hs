{-# OPTIONS_GHC -Wall #-}

module Observable.Interpreter where

import Control.Monad.Free (iterM)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Measurable.Core (Measure)
import qualified Measurable.Measures as Measurable
import Observable.Core
import System.Random.MWC.Probability as MWC

-- | A forward-mode sampling interpreter.  Produces a sample from the
--   model's predictive distribution and returns it in a monadic context.
simulate :: PrimMonad m => Model a -> Gen (PrimState m) -> m a
simulate = sample . iterM alg where
  alg (BinomialF n p next)           = MWC.binomial n p >>= next
  alg (BetaF a b next)               = MWC.beta a b >>= next
  alg (GammaF a b next)              = MWC.gamma a b >>= next
  alg (InvGammaF a b next)           = MWC.inverseGamma a b >>= next
  alg (StandardF next)               = MWC.standard >>= next
  alg (NormalF a b next)             = MWC.normal a b >>= next
  alg (StudentF m k next)            = MWC.t m 1 k >>= next
  alg (UniformF a b next)            = MWC.uniformR (a, b) >>= next
  alg (DirichletF as next)           = MWC.dirichlet as >>= next
  alg (SymmetricDirichletF n a next) = MWC.symmetricDirichlet n a >>= next
  alg (DiscreteUniformF n next)      = MWC.discreteUniform [0..pred n] >>= next
  alg (CategoricalF ps next)         = MWC.categorical ps >>= next
  alg (IsoGaussF ms v next)          = MWC.isoGauss ms v >>= next
  alg (PoissonF l next)              = MWC.poisson l >>= next
  alg (ExponentialF l next)          = MWC.exponential l >>= next

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

















-- so; goal here is to make a forward pass, cache the values for every node
-- so return of forward pass is a set of values at every node
-- then go backwards, calculate measure terms.
--
-- this is more or less what rob has done.
--
-- crux is to get samples first, then use those when going back.
--
-- so the first thing then is to write an interpreter that grabs a sample but
-- preserves it.  i want to propagate it but also keep the individual samples















-- -- | A log posterior score interpreter.
-- logPosterior :: Parameters -> Model a -> Double
-- logPosterior ps =
--       Map.foldl' (+) 0
--     . runIdentity
--     . flip runReaderT ps
--     . flip execStateT mempty
--     . resolve
--   where
--     resolve
--       :: Model a
--       -> StateT (Environment Double) (ReaderT Parameters Identity) a
--     resolve (Pure a) = return a
--     resolve (Free e) = case e of
--       Observe name dist next -> do
--         provided <- fmap (Map.lookup name) (lift ask)
--         let (dynamic, paramScore) = case provided of
--              Nothing -> error "logPosterior: no value provided"
--              Just v  -> score v dist
--         modify $ Map.alter (add paramScore) name
--         let val = case fromDynamic dynamic of
--               Nothing -> error "couldn't cast value"
--               Just x  -> x
--         resolve (next val)

collapse :: Model a -> Model a
collapse = undefined

-- need to look at the structure and rewrite; not sure how to do this offhand
-- but seems to be very easy to do
--
-- Free (BetaF a b (Free (BinomialF n p k)))
-- Free (BetaF (n + a + x)
--




-- | A pretty-printer.  Omits hyperparameter values and bound values at their
--   call sites, and only shows the overall structure of the AST.
-- ast :: Show a => Observable a -> String
-- ast Pure {}  = ""
-- ast (Free f) = case f of
--   Observe name dist next ->
--     let space = case squash next dist of
--           Pure _ -> ""
--           _      -> " "
--     in  "(Observe " <> name <> " (" <> show dist <> ")" <> space
--           <> ast (squash next dist) <> ")"

-- -- | Returns nodes in the syntax tree, ordered from top to bottom.
-- randomVariables :: Observable a -> [String]
-- randomVariables = go [] where
--   go acc Pure {}  = reverse acc
--   go acc (Free f) = case f of
--     Observe name dist next -> go (name : acc) (squash next dist)








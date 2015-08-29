{-# OPTIONS_GHC -Wall #-}

module Observable.Interpreter where

import Control.Applicative (Applicative)
import Control.Monad.Free (iterM)
-- import Control.Monad.Trans.Class (lift)
-- import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
-- import Control.Monad.Trans.State.Strict (execStateT, StateT, modify)
import Control.Monad.Primitive (PrimMonad, PrimState)
-- import Data.Dynamic (fromDynamic)
-- import Data.Functor.Identity (Identity, runIdentity)
-- import qualified Data.Map as Map (alter, foldl', lookup)
-- import Data.Monoid ((<>), mempty)
import Measurable.Core (Measure)
import qualified Measurable.Measures as Measurable
import Observable.Core
-- import Observable.Distribution
-- import Observable.Utils
import System.Random.MWC.Probability as MWC

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

-- | A forward-mode sampling interpreter.  Produces a sample from the joint
--   distribution and returns it in IO.
simulate
  :: (Applicative m, PrimMonad m)
  => Observable a
  -> Gen (PrimState m)
  -> m a
simulate expr = sample (iterM alg expr) where
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

-- | Forward-mode measure interpreter.  Produces a measure according to the
--   joint distribution, but only returns the leaf node of the graph.
forwardMeasure :: Observable a -> Measure a
forwardMeasure = iterM alg where
  alg (BinomialF n p next) = Measurable.binomial n p >>= next
  alg (BetaF a b next)     = Measurable.beta a b >>= next
  alg (GammaF a b next)    = Measurable.gamma a b >>= next
  alg (StandardF next)     = Measurable.standard >>= next
  alg (NormalF a b next)   = Measurable.normal a b >>= next
  alg _ = error "forwardMeasure: distribution not supported"

-- -- | A log posterior score interpreter.
-- logPosterior :: Parameters -> Observable a -> Double
-- logPosterior ps =
--       Map.foldl' (+) 0
--     . runIdentity
--     . flip runReaderT ps
--     . flip execStateT mempty
--     . resolve
--   where
--     resolve
--       :: Observable a
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

-- -- | Condition a model on some data, returning a 'Target'.
-- condition
--   :: Observations
--   -> Observable a
--   -> Target
-- condition xs prog = createTargetWithoutGradient
--   (\ps -> logPosterior (ps <> xs) prog)

-- newtype Conditional a = Conditional a
--
-- condition' :: a -> Observable a -> Conditional a
-- condition' xs expr = eval expr where
--   eval (Pure r) = undefined
--   eval (Free e) = case e of
--     Observe _ dist next -> case dist of
--       Binomial n p           -> eval . next =<< MWC.binomial n p
--       Beta a b               -> eval . next =<< MWC.beta a b
--       Gamma a b              -> eval . next =<< MWC.gamma a b
--       InvGamma a b           -> eval . next =<< MWC.inverseGamma a b
--       Standard               -> eval . next =<< MWC.standard
--       Normal a b             -> eval . next =<< MWC.normal a b
--       Student m k            -> eval . next =<< MWC.t m 1 k
--       Uniform a b            -> eval . next =<< MWC.uniformR (a, b)
--       Dirichlet as           -> eval . next =<< MWC.dirichlet as
--       SymmetricDirichlet n a -> eval . next =<< MWC.symmetricDirichlet n a
--       DiscreteUniform n      -> eval . next =<< MWC.discreteUniform [0..pred n]
--       Categorical ps         -> eval . next =<< MWC.categorical ps
--       IsoGauss ms v          -> eval . next =<< MWC.isoGauss ms v
--       Poisson l              -> eval . next =<< MWC.poisson l
--       Exponential l          -> eval . next =<< MWC.exponential l
--





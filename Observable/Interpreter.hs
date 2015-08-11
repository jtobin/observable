{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTs #-}

module Observable.Interpreter where

import Control.Applicative (Applicative)
import Control.Monad.Free (Free(..))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import Control.Monad.Trans.State.Strict (execStateT, StateT, modify)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Dynamic (fromDynamic)
import Data.Functor.Identity (Identity, runIdentity)
import qualified Data.Map as Map (alter, foldl', lookup)
import Data.Monoid ((<>), mempty)
import Measurable.Core (Measure)
import qualified Measurable.Measures as Measurable
import Observable.Core
import Observable.Distribution
import Observable.Utils
import System.Random.MWC.Probability as MWC

-- | A pretty-printer.  Omits hyperparameter values and bound values at their
--   call sites, and only shows the overall structure of the AST.
ast :: Show a => Observable a -> String
ast Pure {}  = ""
ast (Free f) = case f of
  Observe name dist next ->
    let space = case squash next dist of
          Pure _ -> ""
          _      -> " "
    in  "(Observe " <> name <> " (" <> show dist <> ")" <> space
          <> ast (squash next dist) <> ")"

-- | Returns nodes in the syntax tree, ordered from top to bottom.
randomVariables :: Observable a -> [String]
randomVariables = go [] where
  go acc Pure {}  = reverse acc
  go acc (Free f) = case f of
    Observe name dist next -> go (name : acc) (squash next dist)

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
      Binomial n p           -> eval . next =<< MWC.binomial n p
      Beta a b               -> eval . next =<< MWC.beta a b
      Gamma a b              -> eval . next =<< MWC.gamma a b
      InvGamma a b           -> eval . next =<< MWC.inverseGamma a b
      Standard               -> eval . next =<< MWC.standard
      Normal a b             -> eval . next =<< MWC.normal a b
      Student m k            -> eval . next =<< MWC.t m 1 k
      Uniform a b            -> eval . next =<< MWC.uniformR (a, b)
      Dirichlet as           -> eval . next =<< MWC.dirichlet as
      SymmetricDirichlet n a -> eval . next =<< MWC.symmetricDirichlet n a
      DiscreteUniform n      -> eval . next =<< MWC.discreteUniform [0..pred n]
      Categorical ps         -> eval . next =<< MWC.categorical ps
      IsoGauss ms v          -> eval . next =<< MWC.isoGauss ms v
      Poisson l              -> eval . next =<< MWC.poisson l
      Exponential l          -> eval . next =<< MWC.exponential l

-- | Forward-mode measure interpreter.  Produces a measure according to the
--   joint distribution, but only returns the leaf node of the graph.
forwardMeasure :: Observable a -> Measure a
forwardMeasure = eval where
  eval :: Observable a -> Measure a
  eval (Pure r) = return r
  eval (Free e) = case e of
    Observe _ dist next -> case dist of
      Binomial n p -> eval . next =<< Measurable.binomial n p
      Beta a b     -> eval . next =<< Measurable.beta a b
      Gamma a b    -> eval . next =<< Measurable.gamma a b
      Standard     -> eval . next =<< Measurable.standard
      Normal a b   -> eval . next =<< Measurable.normal a b
      d -> error $ "forwardMeasure: does not support distribution " <> show d

-- | A log posterior score interpreter.
logPosterior :: Parameters -> Observable a -> Double
logPosterior ps =
      Map.foldl' (+) 0
    . runIdentity
    . flip runReaderT ps
    . flip execStateT mempty
    . resolve
  where
    resolve
      :: Observable a
      -> StateT (Environment Double) (ReaderT Parameters Identity) a
    resolve (Pure a) = return a
    resolve (Free e) = case e of
      Observe name dist next -> do
        provided <- fmap (Map.lookup name) (lift ask)
        let (dynamic, paramScore) = case provided of
             Nothing -> error "logPosterior: no value provided"
             Just v  -> score v dist
        modify $ Map.alter (add paramScore) name
        let val = case fromDynamic dynamic of
              Nothing -> error "couldn't cast value"
              Just x  -> x
        resolve (next val)

-- | Condition a model on some data, returning a 'Target'.
condition
  :: Observations
  -> Observable a
  -> Target
condition xs prog = createTargetWithoutGradient
  (\ps -> logPosterior (ps <> xs) prog)


{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTs #-}

module Observable.Interpreter where

import Control.Applicative
import Control.Monad.Free
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict
import Control.Monad.Primitive
import Data.Dynamic
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
      Binomial n p   -> eval . next =<< binomial n p
      Beta a b       -> eval . next =<< beta a b
      Gamma a b      -> eval . next =<< gamma a b
      InvGamma a b   -> eval . next =<< inverseGamma a b
      Standard       -> eval . next =<< standard
      Normal a b     -> eval . next =<< normal a b
      Student m k    -> eval . next =<< t m 1 k
      Uniform a b    -> eval . next =<< uniformR (a, b)

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
      InvGamma a b -> eval . next =<< fromDensityFunction (invGammaDensity a b)
      Standard     -> eval . next =<< Measurable.standard
      Normal a b   -> eval . next =<< Measurable.normal a b
      Student m k  -> eval . next =<< fromDensityFunction (tDensity m 1 k)
      Uniform a b  -> eval . next =<< fromDensityFunction (uniformDensity a b)

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


-- literally; transform the return value, delete any nodes that have


-- | Condition a model on some data.
--
--   @
--     bb = do
--       p <- observe "p" (beta 1 3)
--       observe "x" (binomial 10 p)
--
--     observation = Map.fromList [("x", int 3)]
--
--     bbPosterior = condition bb observation
--   @
--
--   >>> :t bbPosterior
--   bbPosterior :: Environment Lit -> Environment
--
--   >>> bbPosterior (Map.fromList [("p", double 0.3)])
--   -0.9358888787666453
--
condition
  :: Observable a
  -> Parameters
  -> Parameters
  -> Double
condition prog xs ps = logPosterior (ps <> xs) prog


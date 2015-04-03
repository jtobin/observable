
module Observable.Interpreter where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict
import Control.Monad.Primitive
import Data.Functor.Identity
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
import Data.Traversable
import Observable.Core
import Observable.Utils
import System.Random.MWC
import System.Random.MWC.Distributions
import Statistics.Distribution
import qualified Statistics.Distribution.Beta as Statistics
import qualified Statistics.Distribution.Binomial as Statistics
import qualified Statistics.Distribution.Gamma as Statistics
import qualified Statistics.Distribution.Normal as Statistics

-- | A pretty-printing interpreter for visualizing abstract syntax.
pretty :: Program t -> String
pretty e = go e 0 where
  go (Free (Observe name dist next)) j = "(Let " ++ name ++ " " ++ inner ++ ")"
    where
      inner = "(" ++ show dist ++ ") " ++ go (next (LitString name)) (succ j)

  go (Free (Returning (LitString s))) _ = s

-- | A sampling or 'forward-mode' interpreter for our language.
sample :: Program t -> IO Lit
sample e = withSystemRandom . asGenIO $ samp e where
  samp (Free e) g = case e of
    Observe _ dist next -> case dist of

      Binomial en pe -> case (en, pe) of

        (LitInt n, LitDouble p) -> do
          val <- fmap LitInt (binomial n p g)
          samp (next val) g

        (LitDouble n, LitDouble p) -> do
          val <- fmap LitInt (binomial (truncate n) p g)
          samp (next val) g

        _ -> error "type error"

      Multinomial en ps -> case (en, ps) of

        (LitInt n, LitList probs) -> do
          let extract x = case x of
                LitDouble j -> j
                _ -> error "type error (verify)"

              rawProbs = fmap extract probs

          vs <- multinomial n rawProbs g
          let val = list (fmap int vs)
          samp (next val) g

        (LitDouble n, LitList probs) -> do
          let extract x = case x of
                LitDouble j -> j
                _ -> error "type error (verify)"

              rawProbs = fmap extract probs

          vs <- multinomial (truncate n) rawProbs g
          let val = list (fmap int vs)
          samp (next val) g

        _ -> error "type error"

      SymmetricDirichlet en a -> case (en, a) of

        (LitInt n, LitDouble a) -> do
          probs <- symmetricDirichlet n a g
          let val = list (fmap double probs)
          samp (next val) g

        (LitDouble n, LitDouble a) -> do
          probs <- symmetricDirichlet (truncate n) a g
          let val = list (fmap double probs)
          samp (next val) g

        (LitInt n, LitInt a) -> do
          probs <- symmetricDirichlet n (fromIntegral a) g
          let val = list (fmap double probs)
          samp (next val) g

        (LitDouble n, LitInt a) -> do
          probs <- symmetricDirichlet (truncate n) (fromIntegral a) g
          let val = list (fmap double probs)
          samp (next val) g

        _ -> error "type error"

      Gaussian me sd -> case (me, sd) of
        (LitDouble m, LitDouble s) -> do
          val <- fmap LitDouble (normal m s g)
          samp (next val) g

        (LitInt m, LitInt s) -> do
          let (em, es) = (fromIntegral m, fromIntegral s)
          val <- fmap LitDouble (normal em es g)
          samp (next val) g

        (LitInt m, LitDouble s) -> do
          let (em, es) = (fromIntegral m, s)
          val <- fmap LitDouble (normal em es g)
          samp (next val) g

        (LitDouble m, LitInt s) -> do
          let (em, es) = (m, fromIntegral s)
          val <- fmap LitDouble (normal em es g)
          samp (next val) g

        _ -> error "type error"

      StandardGaussian -> do
          val <- fmap LitDouble (standard g)
          samp (next val) g

      IsoGaussian (LitList mes) sd -> case sd of
        LitDouble s -> do
          let indiv element = case element of
                LitDouble m -> fmap LitDouble (normal m s g)
                LitInt m    -> fmap LitDouble (normal (fromIntegral m) s g)
                _ -> error "type error"
          val <- fmap LitList (traverse indiv mes)
          samp (next val) g

        LitInt s -> do
          let sdub = fromIntegral s
              indiv element = case element of
                LitDouble m -> fmap LitDouble (normal m sdub g)
                LitInt m    -> fmap LitDouble (normal (fromIntegral m) sdub g)
                _ -> error "type error"
          val <- fmap LitList (traverse indiv mes)
          samp (next val) g

        _ -> error "Type error"

      Gamma ay be -> case (ay, be) of
        (LitDouble a, LitDouble b) -> do
          val <- fmap LitDouble (gamma a b g)
          samp (next val) g

        (LitInt a, LitInt b) -> do
          let (aye, bee) = (fromIntegral a, fromIntegral b)
          val <- fmap LitDouble (gamma aye bee g)
          samp (next val) g

        (LitDouble a, LitInt b) -> do
          let (aye, bee) = (a, fromIntegral b)
          val <- fmap LitDouble (gamma aye bee g)
          samp (next val) g

        (LitInt a, LitDouble b) -> do
          let (aye, bee) = (fromIntegral a, b)
          val <- fmap LitDouble (gamma aye bee g)
          samp (next val) g

        _ -> error "Type error"

      Beta ay be -> case (ay, be) of
        (LitDouble a, LitDouble b) -> do
          val <- fmap LitDouble (beta a b g)
          samp (next val) g

        (LitInt a, LitInt b) -> do
          let (aye, bee) = (fromIntegral a, fromIntegral b)
          val <- fmap LitDouble (beta aye bee g)
          samp (next val) g

        (LitDouble a, LitInt b) -> do
          let (aye, bee) = (a, fromIntegral b)
          val <- fmap LitDouble (beta aye bee g)
          samp (next val) g

        (LitInt a, LitDouble b) -> do
          let (aye, bee) = (fromIntegral a, b)
          val <- fmap LitDouble (beta aye bee g)
          samp (next val) g

        _ -> error "Type error"

    Returning a -> return a

-- | A posterior or 'backwards-mode' interpreter for our language.  Returns
--   values proportional to the log-posterior probabilities associated with
--   each parameter and observation.
logPosterior :: Map String Lit -> Program t -> Map String Double
logPosterior ps =
      runIdentity
    . flip runReaderT ps
    . flip execStateT mempty
    . resolve
  where
    resolve (Free e) = case e of
      Observe name dist next -> case dist of
        Binomial en pe -> case (en, pe) of
          (LitInt n, LitDouble p) -> do
            store <- lift ask
            let val = fromJust $ Map.lookup name store
                cx  = case val of
                  LitInt j    -> j
                  LitDouble j -> truncate j
                  _ -> error $ "type error, parameter '" <> name <> "'"
                score = log $ probability (Statistics.binomial n p) cx
            modify $ Map.insert name score
            resolve (next (LitInt cx))

          (LitDouble n, LitDouble p) -> do
            store <- lift ask
            let val = fromJust $ Map.lookup name store
                cx  = case val of
                  LitInt j    -> j
                  LitDouble j -> truncate j
                  _ -> error $ "type error, parameter '" <> name <> "'"
                score = log $ probability (Statistics.binomial (truncate n) p) cx
            modify $ Map.insert name score
            resolve (next (LitInt cx))

          _ -> error $ "type error (posterior), " <> show (en, pe)

        Gaussian me sd -> case (me, sd) of
          (LitDouble m, LitDouble s) -> do
            store <- lift ask
            let val = fromJust $ Map.lookup name store
                cx  = case val of
                  LitDouble j -> j
                  LitInt j    -> fromIntegral j
                  _ -> error $ "type error, parameter '" <> name <> "'"
                score = log $ density (Statistics.normalDistr m s) cx
            modify $ Map.insert name score
            resolve (next (LitDouble cx))

          (LitInt m, LitInt s) -> do
            store <- lift ask
            let val = fromJust $ Map.lookup name store
                cx  = case val of
                  LitDouble j -> j
                  LitInt j    -> fromIntegral j
                  _ -> error $ "type error, parameter '" <> name <> "'"
                (em, es)      = (fromIntegral m, fromIntegral s)
                score = log $ density (Statistics.normalDistr em es) cx
            modify $ Map.insert name score
            resolve (next (LitDouble cx))

          (LitDouble m, LitInt s) -> do
            store <- lift ask
            let val = fromJust $ Map.lookup name store
                cx  = case val of
                  LitDouble j -> j
                  LitInt j    -> fromIntegral j
                  _ -> error $ "type error, parameter '" <> name <> "'"
                (em, es)      = (m, fromIntegral s)
                score = log $ density (Statistics.normalDistr em es) cx
            modify $ Map.insert name score
            resolve (next (LitDouble cx))

          (LitInt m, LitDouble s) -> do
            store <- lift ask
            let val = fromJust $ Map.lookup name store
                cx  = case val of
                  LitDouble j -> j
                  LitInt j    -> fromIntegral j
                  _ -> error $ "type error, parameter '" <> name <> "'"
                (em, es)      = (fromIntegral m, s)
                score = log $ density (Statistics.normalDistr em es) cx
            modify $ Map.insert name score
            resolve (next (LitDouble cx))

          _ -> error $ "type error (posterior), " <> show (me, sd)

        StandardGaussian -> do
          store <- lift ask
          let val = fromJust $ Map.lookup name store
              cx  = case val of
                LitDouble j -> j
                LitInt j    -> fromIntegral j
                _ -> error $ "type error, parameter '" <> name <> "'"
              score = log $ density Statistics.standard cx
          modify $ Map.insert name score
          resolve (next (LitDouble cx))

        IsoGaussian (LitList mes) sd -> case sd of
          LitDouble s -> do
            store <- lift ask
            let val = fromJust $ Map.lookup name store
                cxs = case val of
                  LitList xs -> xs
                  _ -> error $ "type error, parameter '" <> name <> "'"
                score (mean, value) = case (mean, value) of
                  (LitDouble m, LitDouble v) ->
                    log $ density (Statistics.normalDistr m s) v
                  (LitInt m, LitDouble v)    ->
                    log $ density (Statistics.normalDistr (fromIntegral m) s) v
                  (LitDouble m, LitInt v)    ->
                    log $ density (Statistics.normalDistr m s) (fromIntegral v)
                  (LitInt m, LitInt v)       ->
                    log $ density (Statistics.normalDistr (fromIntegral m) s) (fromIntegral v)
                  _ -> error $ "type error, parameter '" <> name <> "'"

                result = sum (fmap score (zip mes cxs))
            modify $ Map.insert name result
            resolve (next val)

          LitInt s -> do
            store <- lift ask
            let val = fromJust $ Map.lookup name store
                es  = fromIntegral s
                cxs = case val of
                  LitList xs -> xs
                  _ -> error $ "type error, parameter '" <> name <> "'"
                score (mean, value) = case (mean, value) of
                  (LitDouble m, LitDouble v) ->
                    log $ density (Statistics.normalDistr m es) v
                  (LitInt m, LitDouble v)    ->
                    log $ density (Statistics.normalDistr (fromIntegral m) es) v
                  (LitDouble m, LitInt v)    ->
                    log $ density (Statistics.normalDistr m es) (fromIntegral v)
                  (LitInt m, LitInt v)       ->
                    log $ density (Statistics.normalDistr (fromIntegral m) es) (fromIntegral v)
                  _ -> error $ "type error, parameter '" <> name <> "'"

                result = sum (fmap score (zip mes cxs))
            modify $ Map.insert name result
            resolve (next val)

          _ -> error $ "type error (posterior), " <> show (mes, sd)

        Gamma me sd -> case (me, sd) of
          (LitDouble m, LitDouble s) -> do
            store <- lift ask
            let val = fromJust $ Map.lookup name store
                cx  = case val of
                  LitDouble j -> j
                  LitInt j    -> fromIntegral j
                  _ -> error $ "type error, parameter '" <> name <> "'"
                score = log $ density (Statistics.gammaDistr m s) cx
            modify $ Map.insert name score
            resolve (next (LitDouble cx))

          (LitInt m, LitInt s) -> do
            store <- lift ask
            let val = fromJust $ Map.lookup name store
                cx  = case val of
                  LitDouble j -> j
                  LitInt j    -> fromIntegral j
                  _ -> error $ "type error, parameter '" <> name <> "'"
                (em, es)      = (fromIntegral m, fromIntegral s)
                score = log $ density (Statistics.gammaDistr em es) cx
            modify $ Map.insert name score
            resolve (next (LitDouble cx))

          (LitDouble m, LitInt s) -> do
            store <- lift ask
            let val = fromJust $ Map.lookup name store
                cx  = case val of
                  LitDouble j -> j
                  LitInt j    -> fromIntegral j
                  _ -> error $ "type error, parameter '" <> name <> "'"
                (em, es)      = (m, fromIntegral s)
                score = log $ density (Statistics.gammaDistr em es) cx
            modify $ Map.insert name score
            resolve (next (LitDouble cx))

          (LitInt m, LitDouble s) -> do
            store <- lift ask
            let val = fromJust $ Map.lookup name store
                cx  = case val of
                  LitDouble j -> j
                  LitInt j    -> fromIntegral j
                  _ -> error $ "type error, parameter '" <> name <> "'"
                (em, es)      = (fromIntegral m, s)
                score = log $ density (Statistics.gammaDistr em es) cx
            modify $ Map.insert name score
            resolve (next (LitDouble cx))

          _ -> error $ "type error (posterior), " <> show (me, sd)

        Beta me sd -> case (me, sd) of
          (LitDouble m, LitDouble s) -> do
            store <- lift ask
            let val = fromJust $ Map.lookup name store
                cx  = case val of
                  LitDouble j -> j
                  LitInt j    -> fromIntegral j
                  _ -> error $ "type error, parameter '" <> name <> "'"
                score = log $ density (Statistics.betaDistr m s) cx
            modify $ Map.insert name score
            resolve (next (LitDouble cx))

          (LitInt m, LitInt s) -> do
            store <- lift ask
            let val = fromJust $ Map.lookup name store
                cx  = case val of
                  LitDouble j -> j
                  LitInt j    -> fromIntegral j
                  _ -> error $ "type error, parameter '" <> name <> "'"
                (em, es)      = (fromIntegral m, fromIntegral s)
                score = log $ density (Statistics.betaDistr em es) cx
            modify $ Map.insert name score
            resolve (next (LitDouble cx))

          (LitDouble m, LitInt s) -> do
            store <- lift ask
            let val = fromJust $ Map.lookup name store
                cx  = case val of
                  LitDouble j -> j
                  LitInt j    -> fromIntegral j
                  _ -> error $ "type error, parameter '" <> name <> "'"
                (em, es)      = (m, fromIntegral s)
                score = log $ density (Statistics.betaDistr em es) cx
            modify $ Map.insert name score
            resolve (next (LitDouble cx))

          (LitInt m, LitDouble s) -> do
            store <- lift ask
            let val = fromJust $ Map.lookup name store
                cx  = case val of
                  LitDouble j -> j
                  LitInt j    -> fromIntegral j
                  _ -> error $ "type error, parameter '" <> name <> "'"
                (em, es)      = (fromIntegral m, s)
                score = log $ density (Statistics.betaDistr em es) cx
            modify $ Map.insert name score
            resolve (next (LitDouble cx))

          _ -> error $ "type error (posterior), " <> show (me, sd)

      Returning a -> return a


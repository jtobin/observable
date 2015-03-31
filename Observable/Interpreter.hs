
module Observable.Interpreter where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict
import Control.Monad.Primitive
import Data.Map (Map)
import qualified Data.Map as Map
import Observable.Core
import Observable.Utils
import System.Random.MWC
import System.Random.MWC.Distributions
import Statistics.Distribution
import qualified Statistics.Distribution.Beta as Statistics
import qualified Statistics.Distribution.Binomial as Statistics
import qualified Statistics.Distribution.Gamma as Statistics
import qualified Statistics.Distribution.Normal as Statistics

-- | A simple beta-binomial model for testing.
betaBinomial :: Lit -> Lit -> Lit -> Free (Observable Lit) Lit
betaBinomial n a b = do
  p <- observe "p" (Beta a b)
  x <- observe "x" (Binomial n p)
  returning x

test = do
  p <- observe "p" (Beta 1 8)
  x <- observe "x" (Binomial 10 p)
  returning x

-- | A pretty-printing interpreter for visualizing abstract syntax.
pretty :: Free (Observable Lit) t -> String
pretty e = go e 0 where
  go (Free (Observe name dist next)) j = "(Let " ++ name ++ " " ++ inner ++ ")"
    where
      inner = "(" ++ show dist ++ ") " ++ go (next (LitString name)) (succ j)

  go (Free (Returning (LitString s))) _ = s

-- | A sampling or 'forward-mode' interpreter for our language.
sample :: Free (Observable Lit) t -> IO Lit
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

-- only condition on a single observation to start
posterior x ps (Free e) = case e of
  Observe name dist next -> case dist of
    Binomial en pe ->
      let cx = case x of
            LitInt j    -> j
            LitDouble j -> truncate j
            _ -> error "type error"
      in  case (en, pe) of
            (LitInt n, LitDouble p) -> do
              store <- lift ask
              let givenParamVal = Map.lookup name store
                  score = probability (Statistics.binomial n p) cx
              modify $ Map.insert name score

            (LitDouble n, LitDouble p) -> do
              store <- lift ask
              let givenParamVal = Map.lookup name store
                  score = probability (Statistics.binomial (truncate n) p) cx
              modify $ Map.insert name score

            _ -> error "type error"

    Gaussian me sd ->
      let cx = case x of
            LitInt j    -> fromIntegral j
            LitDouble j -> j
            _ -> error "type error"
      in  case (me, sd) of
            (LitDouble m, LitDouble s) -> do
              store <- lift ask
              let givenParamVal = Map.lookup name store
                  score = density (Statistics.normalDistr m s) cx
              modify $ Map.insert name score

            (LitInt m, LitInt s) -> do
              store <- lift ask
              let givenParamVal = Map.lookup name store
                  (em, es)      = (fromIntegral m, fromIntegral s)
                  score = density (Statistics.normalDistr em es) cx
              modify $ Map.insert name score

            (LitDouble m, LitInt s) -> do
              store <- lift ask
              let givenParamVal = Map.lookup name store
                  (em, es)      = (m, fromIntegral s)
                  score = density (Statistics.normalDistr em es) cx
              modify $ Map.insert name score

            (LitInt m, LitDouble s) -> do
              store <- lift ask
              let givenParamVal = Map.lookup name store
                  (em, es)      = (fromIntegral m, s)
                  score = density (Statistics.normalDistr em es) cx
              modify $ Map.insert name score

            _ -> error "type error"

    Gamma me sd ->
      let cx = case x of
            LitInt j    -> fromIntegral j
            LitDouble j -> j
            _ -> error "type error"
      in  case (me, sd) of
            (LitDouble m, LitDouble s) -> do
              store <- lift ask
              let givenParamVal = Map.lookup name store
                  score = density (Statistics.gammaDistr m s) cx
              modify $ Map.insert name score

            (LitInt m, LitInt s) -> do
              store <- lift ask
              let givenParamVal = Map.lookup name store
                  (em, es)      = (fromIntegral m, fromIntegral s)
                  score = density (Statistics.gammaDistr em es) cx
              modify $ Map.insert name score

            (LitDouble m, LitInt s) -> do
              store <- lift ask
              let givenParamVal = Map.lookup name store
                  (em, es)      = (m, fromIntegral s)
                  score = density (Statistics.gammaDistr em es) cx
              modify $ Map.insert name score

            (LitInt m, LitDouble s) -> do
              store <- lift ask
              let givenParamVal = Map.lookup name store
                  (em, es)      = (fromIntegral m, s)
                  score = density (Statistics.gammaDistr em es) cx
              modify $ Map.insert name score

            _ -> error "type error"

    Beta me sd ->
      let cx = case x of
            LitInt j    -> fromIntegral j
            LitDouble j -> j
            _ -> error "type error"
      in  case (me, sd) of
            (LitDouble m, LitDouble s) -> do
              store <- lift ask
              let givenParamVal = Map.lookup name store
                  score = density (Statistics.betaDistr m s) cx
              modify $ Map.insert name score

            (LitInt m, LitInt s) -> do
              store <- lift ask
              let givenParamVal = Map.lookup name store
                  (em, es)      = (fromIntegral m, fromIntegral s)
                  score = density (Statistics.betaDistr em es) cx
              modify $ Map.insert name score

            (LitDouble m, LitInt s) -> do
              store <- lift ask
              let givenParamVal = Map.lookup name store
                  (em, es)      = (m, fromIntegral s)
                  score = density (Statistics.betaDistr em es) cx
              modify $ Map.insert name score

            (LitInt m, LitDouble s) -> do
              store <- lift ask
              let givenParamVal = Map.lookup name store
                  (em, es)      = (fromIntegral m, s)
                  score = density (Statistics.betaDistr em es) cx
              modify $ Map.insert name score

            _ -> error "type error"

  Returning a -> return a



--  Observe name dist next -> case dist of
--
--    Binomial en pe -> case (en, pe) of
--
--      (LitInt n, LitDouble p) -> do
--        val <- fmap LitInt (binomial n p g)
--        samp (next val) g
--
--      (LitDouble n, LitDouble p) -> do
--        val <- fmap LitInt (binomial (truncate n) p g)
--        samp (next val) g
--
--      _ -> error "type error"
--
--    Gaussian me sd -> case (me, sd) of
--      (LitDouble m, LitDouble s) -> do
--        val <- fmap LitDouble (normal m s g)
--        samp (next val) g
--
--      (LitInt m, LitInt s) -> do
--        let (em, es) = (fromIntegral m, fromIntegral s)
--        val <- fmap LitDouble (normal em es g)
--        samp (next val) g
--
--      _ -> error "Type error"
--
--    Gamma ay be -> case (ay, be) of
--      (LitDouble a, LitDouble b) -> do
--        val <- fmap LitDouble (gamma a b g)
--        samp (next val) g
--
--      (LitInt a, LitInt b) -> do
--        let (aye, bee) = (fromIntegral a, fromIntegral b)
--        val <- fmap LitDouble (gamma aye bee g)
--        samp (next val) g
--
--      _ -> error "Type error"
--
--    Beta ay be -> case (ay, be) of
--      (LitDouble a, LitDouble b) -> do
--        val <- fmap LitDouble (beta a b g)
--        samp (next val) g
--
--      (LitInt a, LitInt b) -> do
--        let (aye, bee) = (fromIntegral a, fromIntegral b)
--        val <- fmap LitDouble (beta aye bee g)
--        samp (next val) g
--
--      _ -> error "Type error"
--
--  Returning a -> return a


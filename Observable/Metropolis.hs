
module Observable.Metropolis where

import Control.Monad
import Control.Monad.Free
import Control.Monad.Primitive
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import qualified Data.Foldable as Foldable
import Data.Functor.Identity
import Data.Dynamic
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Monoid
import Observable.Core
import Observable.Distribution
import qualified System.Random.MWC.Probability as P

data Chain a = Chain {
    target :: Target
  , params :: Map String Dynamic
  }

type Transition m a = StateT (Chain a) (P.Prob m) (Map String Dynamic)

condition
  :: Model a
  -> Map String Dynamic
  -> Target
condition prog xs = createTargetWithoutGradient
  (\ps -> logPosterior (ps <> xs) prog)

logPosterior :: Map String Dynamic -> Model a -> Double
logPosterior ps =
      Map.foldl' (+) 0
    . runIdentity
    . flip runReaderT ps
    . flip execStateT mempty
    . score
  where
    score
      :: Model a
      -> StateT (Map String Double) (ReaderT (Map String Dynamic) Identity) a
    score = iterM alg where
      alg (BetaF a b tag k) = do
        provided <- fmap (Map.lookup tag) (lift ask)
        let (v, score) = case provided of
              Nothing -> error $ "logPosterior: no value provided for " ++ tag
              Just x  -> (x, log (densityBeta a b (fromDyn x (error "beta"))))
        modify (Map.alter (add score) tag)
        k (fromDyn v (error "beta score"))

      alg (BinomialF n p tag k) = do
        provided <- fmap (Map.lookup tag) (lift ask)
        let (v, score) = case provided of
              Nothing -> error $ "logPosterior: no value provided for " ++ tag
              Just x  -> (x, log (densityBinomial n p (fromDyn x (error "binom"))))
        modify (Map.alter (add score) tag)
        k (fromDyn v (error "binom score"))

      alg (NormalF m s tag k) = do
        provided <- fmap (Map.lookup tag) (lift ask)
        let (v, score) = case provided of
              Nothing -> error $ "logPosterior: no value provided for " ++ tag
              Just x  -> (x, log (densityNormal m s (fromDyn x (error "normal"))))
        modify (Map.alter (add score) tag)
        k (fromDyn v (error "binom score"))

      alg (GammaF a b tag k) = do
        provided <- fmap (Map.lookup tag) (lift ask)
        let (v, score) = case provided of
              Nothing -> error $ "logPosterior: no value provided for " ++ tag
              Just x  -> (x, log (densityGamma a b (fromDyn x (error "gamma"))))
        modify (Map.alter (add score) tag)
        k (fromDyn v (error "binom score"))

      alg (UniformF a b tag k) = do
        provided <- fmap (Map.lookup tag) (lift ask)
        let (v, score) = case provided of
              Nothing -> error $ "logPosterior: no value provided for " ++ tag
              Just x  -> (x, log (densityUniform a b (fromDyn x (error "uniform"))))
        modify (Map.alter (add score) tag)
        k (fromDyn v (error "binom score"))

      alg (InvGammaF a b tag k) = do
        provided <- fmap (Map.lookup tag) (lift ask)
        let (v, score) = case provided of
              Nothing -> error $ "logPosterior: no value provided for " ++ tag
              Just x  -> (x, log (densityInvGamma a b (fromDyn x (error "invgamma"))))
        modify (Map.alter (add score) tag)
        k (fromDyn v (error "binom score"))

      alg (SymmetricDirichletF _ a tag k) = do
        provided <- fmap (Map.lookup tag) (lift ask)
        let (v, score) = case provided of
              Nothing -> error $ "logPosterior: no value provided for " ++ tag
              Just x  -> (x, log (densitySymmetricDirichlet a (fromDyn x (error "symdir"))))
        modify (Map.alter (add score) tag)
        k (fromDyn v (error "binom score"))

metropolisHastings :: PrimMonad m => Double -> Transition m Double
metropolisHastings e = do
  Chain target current <- get
  proposed <- lift $ perturb current e
  zc       <- lift P.uniform
  let next = nextState target current proposed e zc
  put $ Chain target next
  return next

perturb
  :: PrimMonad m
  => Map String Dynamic
  -> Double
  -> P.Prob m (Map String Dynamic)
perturb q sd = traverse
  (\m -> fmap toDyn (P.normal (fromDyn m (error "perturb")) sd)) q

acceptRatio
  :: Target -> Map String Dynamic -> Map String Dynamic -> Double -> Double
acceptRatio target current proposed sd = exp . min 0 $
    lTarget target proposed + log (sphericalGaussian current proposed sd)
  - lTarget target current  - log (sphericalGaussian proposed current sd)

nextState
  :: Target
  -> Map String Dynamic
  -> Map String Dynamic
  -> Double
  -> Double
  -> Map String Dynamic
nextState target current proposed sd z
    | z < acceptProb = proposed
    | otherwise      = current
  where
    ratio = acceptRatio target current proposed sd
    acceptProb | isNaN ratio = 0
               | otherwise   = ratio

sphericalGaussian :: Map String Dynamic -> Map String Dynamic -> Double -> Double
sphericalGaussian xs mu sd = product $ zipWith ($) normalDists xsAsList
  where
    xsAsList    = map (\x -> fromDyn x (error "xsAsList")) $ Foldable.toList xs
    muAsList    = map (\x -> fromDyn x (error "muAsList")) $ Foldable.toList mu
    normalDists = map (densityNormal sd) muAsList

mcmc
  :: PrimMonad m
  => Double
  -> Int
  -> Chain Double
  -> P.Gen (PrimState m)
  -> m [[Double]]
mcmc e n o g = do
  samples <- P.sample (replicateM n (metropolisHastings e) `evalStateT` o) g
  let raw = fmap (fmap unsafeFromDyn . Foldable.toList) samples
  return raw

unsafeFromDyn :: Typeable a => Dynamic -> a
unsafeFromDyn = fromJust . fromDynamic

add :: Num a => a -> Maybe a -> Maybe a
add v Nothing    = Just v
add v0 (Just v1) = Just (v0 + v1)


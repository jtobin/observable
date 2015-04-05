{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Observable.Core where

import Control.Monad.Free
import Data.Dynamic
import Data.Map (Map)
import Data.Monoid

type Environment a = Map String a

type Parameters = Environment Dynamic

-- | @Observable@ terms.
data ObservableF :: * -> * where
  Observe :: String -> Distribution a -> (a -> r) -> ObservableF r

instance Functor ObservableF where
  fmap f (Observe s d k) = Observe s d (f . k)

-- | @Observable@ programs.
type Observable = Free ObservableF

-- | 'Observe' constructor.
observe :: String -> Distribution a -> Observable a
observe name dist = liftF (Observe name dist id)

-- | Supported probability distributions.
data Distribution :: * -> * where
  Beta        :: Double -> Double -> Distribution Double
  Binomial    :: Int -> Double -> Distribution Int
  Standard    :: Distribution Double
  Normal      :: Double -> Double -> Distribution Double
  Student     :: Double -> Double -> Distribution Double
  Gamma       :: Double -> Double -> Distribution Double
  InvGamma    :: Double -> Double -> Distribution Double

instance Show a => Show (Distribution a) where
  show (Beta a b)     = "Beta " <> show a <> " " <> show b
  show (Binomial a b) = "Binomial " <> show a <> " " <> show b
  show Standard       = "Standard"
  show (Normal a b)   = "Normal " <> show a <> show b
  show (Student a b)  = "Student " <> show a <> show b
  show (Gamma a b)    = "Gamma " <> show a <> " " <> show b
  show (InvGamma a b) = "InvGamma " <> show a <> " " <> show b

beta :: Double -> Double -> Distribution Double
beta = Beta

binomial :: Int -> Double -> Distribution Int
binomial = Binomial

standard :: Distribution Double
standard = Standard

normal :: Double -> Double -> Distribution Double
normal = Normal

student :: Double -> Double -> Distribution Double
student = Student

gamma :: Double -> Double -> Distribution Double
gamma = Gamma

invGamma :: Double -> Double -> Distribution Double
invGamma = InvGamma

int :: Int -> Dynamic
int = toDyn

double :: Double -> Dynamic
double = toDyn

vector :: (Num a, Typeable a) => [a] -> Dynamic
vector = toDyn

parameter :: Typeable a => a -> Dynamic
parameter = toDyn


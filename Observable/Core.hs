{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Observable.Core where

import Control.Monad.Free
import Data.Dynamic
import Data.Map (Map)

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
  Uniform     :: Double -> Double -> Distribution Double

instance Show (Distribution a) where
  show Beta {}     = "Beta _ _"
  show Binomial {} = "Binomial _ _"
  show Standard    = "Standard"
  show Normal {}   = "Normal _ _"
  show Student {}  = "Student _ _"
  show Gamma {}    = "Gamma _ _"
  show InvGamma {} = "InvGamma _ _"
  show Uniform {}  = "Uniform _ _"

-- distribution constructor aliases

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

uniform :: Double -> Double -> Distribution Double
uniform = Uniform

-- toDyn aliases

int :: Int -> Dynamic
int = toDyn

double :: Double -> Dynamic
double = toDyn

vector :: (Num a, Typeable a) => [a] -> Dynamic
vector = toDyn


{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Observable.Core where

import Control.Monad.Free
import Data.Map (Map)
import Data.Typeable
import Observable.Distribution

type Environment a = Map String a

-- | @Observable@ terms.
data ObservableF :: * -> * where
  Observe
    :: Typeable a => String -> Distribution a -> (a -> r) -> ObservableF r

instance Functor ObservableF where
  fmap f (Observe s d k) = Observe s d (f . k)

-- | @Observable@ programs.
type Observable = Free ObservableF

-- | 'Observe' constructor.
observe :: Typeable a => String -> Distribution a -> Observable a
observe name dist = liftF (Observe name dist id)

-- | Parameter type.
data Parameter =
    Continuous Double
  | Discrete Int
  | Vector [Double]
  deriving (Show, Eq)

type Parameters = Environment Parameter

-- parameter constructors

discrete :: Int -> Parameter
discrete = Discrete

continuous :: Double -> Parameter
continuous = Continuous

vector :: [Double] -> Parameter
vector = Vector

-- result of conditioning a program

data Target = Target {
    logTarget :: Environment Parameter -> Double
  , glTarget  :: Maybe (Environment Parameter -> Environment Parameter)
  }

-- smart constructors for distributions

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


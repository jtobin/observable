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
  | ContinuousVector [Double]
  | DiscreteVector [Int]
  deriving (Show, Eq)

type Parameters = Environment Parameter

type Observations = Environment Parameter

-- parameter constructors

discrete :: Int -> Parameter
discrete = Discrete

continuous :: Double -> Parameter
continuous = Continuous

continuousVector :: [Double] -> Parameter
continuousVector = ContinuousVector

discreteVector :: [Int] -> Parameter
discreteVector = DiscreteVector

-- result of conditioning a program

data Target = Target {
    logTarget :: Parameters -> Double
  , glTarget  :: Maybe (Parameters -> Parameters)
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

dirichlet :: [Double] -> Distribution [Double]
dirichlet = Dirichlet

symmetricDirichlet :: Int -> Double -> Distribution [Double]
symmetricDirichlet = SymmetricDirichlet

categorical :: [Double] -> Distribution Int
categorical = Categorical

discreteUniform :: Int -> Distribution Int
discreteUniform = DiscreteUniform

isoGauss :: [Double] -> Double -> Distribution [Double]
isoGauss = IsoGauss

poisson :: Double -> Distribution Int
poisson = Poisson

exponential :: Double -> Distribution Double
exponential = Exponential


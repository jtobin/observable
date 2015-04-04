{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Observable.Core where

import Control.Applicative
import Control.Monad
import Data.Map (Map)
import Data.Monoid

type Environment a = Map String a

-- | GADT-constructed free monad type.
data Free :: (* -> *) -> * -> * where
  Pure :: a -> Free f a
  Free :: f (Free f a) -> Free f a
  deriving Functor

instance Functor f => Applicative (Free f) where
  pure  = return
  (<*>) = ap

instance Functor f => Monad (Free f) where
  return = Pure
  Free c >>= f = Free (fmap (>>= f) c)
  Pure x >>= f = f x

deriving instance (Show (f (Free f a)), Show a) => Show (Free f a)

-- | Embed an action into a Free.
liftF :: Functor f => f a -> Free f a
liftF action = Free (fmap Pure action)

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
  show (Beta a b)        = "Beta " <> show a <> " " <> show b
  show (Binomial a b)    = "Binomial " <> show a <> " " <> show b
  show Standard          = "Standard"
  show (Normal a b)      = "Normal " <> show a <> show b
  show (Student a b)     = "Student " <> show a <> show b
  show (Gamma a b)       = "Gamma " <> show a <> " " <> show b
  show (InvGamma a b)    = "InvGamma " <> show a <> " " <> show b

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

-- | Wrapped literal values required for passing information to the
--   logPosterior interpreter.
data Lit =
    LitInt Int
  | LitDouble Double
  | LitVec [Lit]
  deriving Eq

instance Show Lit where
  show (LitInt j)    = "Int " <> show j
  show (LitDouble j) = "Double " <> show j
  show (LitVec j)    = "Vector " <> show j

int :: Int -> Lit
int = LitInt

double :: Double -> Lit
double = LitDouble

vector :: [Lit] -> Lit
vector = LitVec


{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Observable.Core (
    module Data.Sampling.Types
  , ObservableF(..)
  , Observable
  , observe
  ) where

import Control.Monad.Free (Free, liftF)
import Data.Sampling.Types
import Data.Typeable (Typeable)
import Observable.Distribution

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


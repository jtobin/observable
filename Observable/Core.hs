{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Observable.Core where

import Control.Applicative
import Control.Monad

-- | The free monad type.
data Free f a =
    Free (f (Free f a))
  | Pure a
  deriving Functor

-- | Alias for the Free data constructor.
free :: f (Free f a) -> Free f a
free = Free

-- | Lift an action into a free monad.
liftF :: Functor f => f a -> Free f a
liftF action = Free (fmap Pure action)

instance Functor f => Applicative (Free f) where
  pure  = Pure
  (<*>) = ap

instance Functor f => Monad (Free f) where
  return         = Pure
  Free a >>= f   = free (fmap (>>= f) a)
  Pure a >>= f = f a

deriving instance (Show (f (Free f a)), Show a) => Show (Free f a)

-- | Terms for our simple probabilistic programming language.
--
--   'Observe' binds the (named) result of a distribution to a term, and
--   'Returning' passes back a value as a result.
data Observable a e =
    Observe String Distribution (a -> e)
  | Returning a
  deriving Functor

-- | The 'Observe' term lifted into the free monad.
observe :: String -> Distribution -> Free (Observable a) a
observe name dist = liftF (Observe name dist id)

-- | The 'Returning' term lifted into the free monad.
returning :: a -> Free (Observable a) a
returning = liftF . Returning

-- | Literal values for our simple language.
data Lit =
    LitInt Int
  | LitBool Bool
  | LitDouble Double
  | LitPair (Lit, Lit)
  | LitList [Lit]
  | LitString String
  deriving Eq

instance Show Lit where
  show (LitInt j)       = show j
  show (LitBool j)      = show j
  show (LitDouble j)    = show j
  show (LitPair (i, j)) = show (i, j)
  show (LitList xs)     = show xs
  show (LitString s)    = s

instance Num Lit where
  e0 + e1 = case (e0, e1) of
    (LitInt i, LitInt j)       -> LitInt (i + j)
    (LitDouble i, LitDouble j) -> LitDouble (i + j)
    (LitInt i, LitDouble j)    -> LitDouble (fromIntegral i + j)
    (LitDouble i, LitInt j)    -> LitDouble (i + fromIntegral j)
    _ -> error "type error"

  e0 - e1 = case (e0, e1) of
    (LitInt i, LitInt j)       -> LitInt (i - j)
    (LitDouble i, LitDouble j) -> LitDouble (i - j)
    (LitInt i, LitDouble j)    -> LitDouble (fromIntegral i - j)
    (LitDouble i, LitInt j)    -> LitDouble (i - fromIntegral j)
    _ -> error "type error"

  e0 * e1 = case (e0, e1) of
    (LitInt i, LitInt j)       -> LitInt (i * j)
    (LitDouble i, LitDouble j) -> LitDouble (i * j)
    (LitInt i, LitDouble j)    -> LitDouble (fromIntegral i * j)
    (LitDouble i, LitInt j)    -> LitDouble (i * fromIntegral j)
    _ -> error "type error"

  abs e = case e of
    LitInt j    -> LitInt (abs j)
    LitDouble j -> LitDouble (abs j)
    LitBool _   -> error "type error"

  signum e = case e of
    LitInt j    -> LitInt (signum j)
    LitDouble j -> LitDouble (signum j)
    LitBool _   -> error "type error"

  fromInteger = LitDouble . fromInteger

instance Fractional Lit where
  e0 / e1 = case (e0, e1) of
    (LitInt i, LitInt j) -> LitDouble (fromIntegral i / fromIntegral j)
    (LitDouble i, LitDouble j) -> LitDouble (i / j)
    (LitInt i, LitDouble j) -> LitDouble (fromIntegral i / j)
    (LitDouble i, LitInt j) -> LitDouble (i / fromIntegral j)
    _ -> error "type error"

  recip e = case e of
    LitInt i    -> LitDouble (recip (fromIntegral i))
    LitDouble i -> LitDouble (recip i)
    _ -> error "type error"

  fromRational = LitDouble . fromRational

-- | An abstract probability distribution type.
data Distribution =
    Binomial Lit Lit
  | Gaussian Lit Lit
  | Gamma Lit Lit
  | Beta Lit Lit
  deriving (Eq, Show)


{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Observable.Core where

import Control.Applicative
import Control.Monad
import Data.Map (Map)
import Data.Monoid

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

-- | An alias for programs written in our language.
type Program a = Free (Observable Lit) a

-- | Literal values for our simple language.
data Lit =
    LitInt Int
  | LitBool Bool
  | LitDouble Double
  | LitPair (Lit, Lit)
  | LitList [Lit]
  | LitString String
  | LitEnv (Map String Lit)
  | Nil
  deriving Eq

instance Show Lit where
  show (LitInt j)       = show j
  show (LitBool j)      = show j
  show (LitDouble j)    = show j
  show (LitPair (i, j)) = show (i, j)
  show (LitList xs)     = show xs
  show (LitString s)    = s
  show (LitEnv m)       = show m
  show Nil              = "Nil"

instance Num Lit where
  e0 + e1 = case (e0, e1) of
    (LitInt i, LitInt j)       -> LitInt (i + j)
    (LitDouble i, LitDouble j) -> LitDouble (i + j)
    (LitInt i, LitDouble j)    -> LitDouble (fromIntegral i + j)
    (LitDouble i, LitInt j)    -> LitDouble (i + fromIntegral j)
    _ -> error $ "type error, " <> show e0 <> " + " <> show e1

  e0 - e1 = case (e0, e1) of
    (LitInt i, LitInt j)       -> LitInt (i - j)
    (LitDouble i, LitDouble j) -> LitDouble (i - j)
    (LitInt i, LitDouble j)    -> LitDouble (fromIntegral i - j)
    (LitDouble i, LitInt j)    -> LitDouble (i - fromIntegral j)
    _ -> error $ "type error, " <> show e0 <> " - " <> show e1

  e0 * e1 = case (e0, e1) of
    (LitInt i, LitInt j)       -> LitInt (i * j)
    (LitDouble i, LitDouble j) -> LitDouble (i * j)
    (LitInt i, LitDouble j)    -> LitDouble (fromIntegral i * j)
    (LitDouble i, LitInt j)    -> LitDouble (i * fromIntegral j)
    _ -> error $ "type error, " <> show e0 <> " * " <> show e1

  abs e = case e of
    LitInt j    -> LitInt (abs j)
    LitDouble j -> LitDouble (abs j)
    LitBool _   -> error $ "type error, " <> show e

  signum e = case e of
    LitInt j    -> LitInt (signum j)
    LitDouble j -> LitDouble (signum j)
    LitBool _   -> error $ "type error, " <> show e

  fromInteger = LitDouble . fromInteger

instance Fractional Lit where
  e0 / e1 = case (e0, e1) of
    (LitInt i, LitInt j) -> LitDouble (fromIntegral i / fromIntegral j)
    (LitDouble i, LitDouble j) -> LitDouble (i / j)
    (LitInt i, LitDouble j) -> LitDouble (fromIntegral i / j)
    (LitDouble i, LitInt j) -> LitDouble (i / fromIntegral j)
    _ -> error $ "type error, " <> show e0 <> " / " <> show e1

  recip e = case e of
    LitInt i    -> LitDouble (recip (fromIntegral i))
    LitDouble i -> LitDouble (recip i)
    _ -> error $ "type error, " <> show e

  fromRational = LitDouble . fromRational

list :: [Lit] -> Lit
list = LitList

double :: Double -> Lit
double = LitDouble

int :: Int -> Lit
int = LitInt

-- | An abstract probability distribution type.
data Distribution =
    Binomial Lit Lit
  | StandardGaussian
  | Gaussian Lit Lit
  | IsoGaussian Lit Lit
  | Gamma Lit Lit
  | Beta Lit Lit
  | SymmetricDirichlet Lit Lit
  | Multinomial Lit Lit
  deriving (Eq, Show)


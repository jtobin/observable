{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}

module Observable.Proto where

import Control.Monad.Free
import Control.Comonad.Cofree
import Data.Void
import Prelude hiding (Integer, Float)

import Data.Monoid

newtype Integer = Integer Int deriving (Eq, Show)
newtype Numeric = Numeric Double deriving (Eq, Show)

data Expr =
    IntL Int
  | DoubleL Double
  | IntV Integer
  | DoubleV Numeric

instance Show Expr where
  show (IntL i) = show i
  show (DoubleL d) = show d
  show (IntV e) = show e
  show (DoubleV e) = show e

data ModelF k =
    BetaF Expr Expr (Numeric -> k)
  | BinomialF Expr Expr (Integer -> k)
  | ConditionF
  deriving Functor

type Model = Free ModelF Expr

beta :: Expr -> Expr -> Model
beta a b = liftF (BetaF a b DoubleV)

binomial :: Expr -> Expr -> Model
binomial n p = liftF (BinomialF n p IntV)

numeric :: Double -> Expr
numeric = DoubleL

int :: Int -> Expr
int = IntL

prog :: Model
prog = do
  p <- beta (numeric 1) (numeric 2)
  binomial (int 10) p

display :: Model -> String
display = go 0 where
  go _ (Pure a) = show a
  go i (Free (BetaF a b k)) = "var" <> show i <> " <- beta " <> show a <> " " <> show b <> "\n" <> go (succ i) (k (Numeric (fromIntegral i)))
  go i (Free (BinomialF n p k)) = "var" <> show i <> " <- binomial " <> show n <> " " <> show p <> "\n" <> go (succ i) (k (Integer i))
-- program :: Model Void
-- program = prog >> liftF ConditionF






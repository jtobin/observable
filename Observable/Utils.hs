{-# LANGUAGE NoMonomorphismRestriction #-}

module Observable.Utils where

import Data.Monoid
import qualified Data.Map as Map
import Observable.Core

extractInt :: String -> Environment Lit -> Int
extractInt name store = case Map.lookup name store of
  Nothing -> error $ "parameter '" <> name <> "' not found"
  Just v  -> case v of
    LitInt j -> j
    _ -> error $
      "expected Integer for parameter '" <> name <> "', got: " <> show v

extractDouble :: String -> Environment Lit -> Double
extractDouble name store = case Map.lookup name store of
  Nothing -> error $ "parameter '" <> name <> "' not found"
  Just v  -> case v of
    LitDouble j -> j
    _ -> error $
      "expected Double for parameter '" <> name <> "', got: " <> show v

extractVec :: String -> Environment Lit -> [Lit]
extractVec name store = case Map.lookup name store of
  Nothing -> error $ "parameter '" <> name <> "' not found"
  Just v  -> case v of
    LitVec j -> j
    _ -> error $
      "expected Vector for parameter '" <> name <> "', got: " <> show v

grabDouble :: String -> Lit -> Double
grabDouble _ (LitDouble j) = j
grabDouble name _ = error $ "expected Double for value inside '" <> name <> "'"


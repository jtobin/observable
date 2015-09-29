{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Observable.Distribution where

densityInvGamma :: Floating a => a -> a -> a -> a
densityInvGamma a b x = x ** (negate a - 1) * exp (negate b / x)

densityStudent :: Floating a => a -> a -> a -> a -> a
densityStudent m s v x =
    (1 + recip v * (x - m) ^ 2 / s) ** (negate (v + 1) / 2)

densityUniform :: (Ord a, Fractional a) => a -> a -> a -> a
densityUniform a b x
  | x < a || x > b = 0
  | otherwise      = 1 / (b - a)

densityBinomial :: (Ord a, Num a, Integral b) => b -> a -> b -> a
densityBinomial n p x
  | x < 0 || x > n = 0
  | p < 0 || p > 1 = 0
  | otherwise = p ^ x * (1 - p) ^ (n - x)

densityNormal :: (Ord a, Floating a) => a -> a -> a -> a
densityNormal m s x
  | s < 0     = 0
  | otherwise = recip s * exp (negate (x - m) ^ 2 / (2 * s ^ 2))

densityStandard :: (Ord a, Floating a) => a -> a
densityStandard = densityNormal 0 1

densityGamma :: (Ord a, Floating a) => a -> a -> a -> a
densityGamma a b x
  | a < 0 || b < 0 = 0
  | otherwise      = x ** (a - 1) * exp (negate x / b)

densityBeta :: (Ord a, Floating a) => a -> a -> a -> a
densityBeta a b x
  | x <= 0 || x >= 1 = 0
  | otherwise = x ** (a - 1) * (1 - x) ** (b - 1)

densityDirichlet :: Floating a => [a] -> [a] -> a
densityDirichlet as ts = product $ zipWith (**) ts las where
  las = fmap (subtract 1) as

densitySymmetricDirichlet :: Floating a => a -> [a] -> a
densitySymmetricDirichlet a ts = product $ zipWith (**) ts las where
  las = fmap (subtract 1) (repeat a)

densityDiscreteUniform :: (Integral a, Fractional b) => a -> b
densityDiscreteUniform = recip . fromIntegral

densityCategorical :: (Floating t, Integral a) => [t] -> a -> t
densityCategorical ts x = case safeAt x Nothing ts of
    Nothing -> err
    Just h  -> h
  where
    err = error "densityCategorical: index out of range"
    safeAt _ acc []     = acc
    safeAt j acc (i:is)
      | j < 0     = err
      | j == 0    = Just i
      | otherwise = safeAt (pred j) acc is

densityIsoGauss :: (Ord a, Floating a) => [a] -> a -> [a] -> a
densityIsoGauss [] _ _  = error "unIsoGauss: empty mean vector"
densityIsoGauss _ _ []  = error "unIsoGauss: empty value vector"
densityIsoGauss ms v xs = exp (sum lprobs) where
  lprobs = fmap indiv pairs
  indiv  = uncurry (\m x -> log (densityNormal m v x))
  pairs  = zip ms xs

densityPoisson :: (Floating a, Integral b) => a -> b -> a
densityPoisson l x = l ^ x * fromIntegral (product [1..x])

densityExponential :: Floating a => a -> a -> a
densityExponential l x = x ** negate l



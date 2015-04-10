{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Observable.Distribution where

-- | Supported probability distributions.
data Distribution :: * -> * where
  Beta               :: Double -> Double -> Distribution Double
  Binomial           :: Int -> Double -> Distribution Int
  Standard           :: Distribution Double
  Normal             :: Double -> Double -> Distribution Double
  Student            :: Double -> Double -> Distribution Double
  Gamma              :: Double -> Double -> Distribution Double
  InvGamma           :: Double -> Double -> Distribution Double
  Uniform            :: Double -> Double -> Distribution Double
  Dirichlet          :: [Double] -> Distribution [Double]
  SymmetricDirichlet :: Int -> Double -> Distribution [Double]
  Categorical        :: [Double] -> Distribution Int
  DiscreteUniform    :: Int -> Distribution Int
  IsoGauss           :: [Double] -> Double -> Distribution [Double]
  Poisson            :: Double -> Distribution Int
  Exponential        :: Double -> Distribution Double

instance Show (Distribution a) where
  show Beta {}               = "Beta _ _"
  show Binomial {}           = "Binomial _ _"
  show Standard              = "Standard"
  show Normal {}             = "Normal _ _"
  show Student {}            = "Student _ _"
  show Gamma {}              = "Gamma _ _"
  show InvGamma {}           = "InvGamma _ _"
  show Uniform {}            = "Uniform _ _"
  show Dirichlet {}          = "Dirichlet _"
  show SymmetricDirichlet {} = "SymmetricDirichlet _ _"
  show Categorical {}        = "Categorical _"
  show DiscreteUniform {}    = "DiscreteUniform _"
  show IsoGauss {}           = "IsoGauss _ _"
  show Poisson {}            = "Poisson _"
  show Exponential {}        = "Exponential _"

-- smart constructors

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

-- (possibly unnormalized) densities

densityInvGamma :: Floating a => a -> a -> a -> a
densityInvGamma a b x = x ** (negate a - 1) * exp (negate b / x)

densityStudent :: Floating a => a -> a -> a -> a -> a
densityStudent m s v x =
    (1 + recip v * (x - m) ^ 2 / s) ** (negate (v + 1) / 2)

densityUniform :: (Ord a, Fractional a) => a -> a -> a -> a
densityUniform a b x
  | x < a || x > b = 0
  | otherwise      = 1 / (b - a)

densityBinomial :: (Num a, Integral b) => b -> a -> b -> a
densityBinomial n p x = p ^ x * (1 - p) ^ (n - x)

densityNormal :: Floating a => a -> a -> a -> a
densityNormal m s x = recip s * exp (negate (x - m) ^ 2 / (2 * s ^ 2))

densityStandard :: Floating a => a -> a
densityStandard = densityNormal 0 1

densityGamma :: Floating a => a -> a -> a -> a
densityGamma a b x = x ** (a - 1) * exp (negate x / b)

densityBeta :: Floating a => a -> a -> a -> a
densityBeta a b x = x ** (a - 1) * (1 - x) ** (b - 1)

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

densityIsoGauss :: Floating a => [a] -> a -> [a] -> a
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


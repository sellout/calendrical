{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-commercial
--
-- Adds missing numeric instances.
module Numeric.Orphans () where

import "base" Control.Applicative (Applicative, liftA2, pure)
import "base" Control.Category ((.))
import "base" Data.Complex (Complex ((:+)))
import "base" Data.Functor (fmap)
import "base" Data.Monoid (Alt (Alt), Ap, Product (Product), Sum (Sum))
import "base" Data.Ord (Down (Down))
import "base" Data.Semigroup (Max (Max), Min (Min))
import "base" Foreign.C.Types (CDouble, CFloat)
import "base" Prelude
  ( Bounded,
    Double,
    Enum,
    Float,
    Floating,
    Fractional,
    Integral,
    Real,
    RealFloat,
    RealFrac,
    acos,
    acosh,
    asin,
    asinh,
    atan,
    atanh,
    cos,
    cosh,
    encodeFloat,
    exp,
    floatDigits,
    floatRadix,
    floatRange,
    fromRational,
    log,
    maxBound,
    minBound,
    pi,
    recip,
    sin,
    sinh,
    (-),
    (/),
    (^),
  )
#if !MIN_VERSION_base(4, 19, 0)
import "base" Prelude (Num)
#endif
#if !MIN_VERSION_base(4, 20, 0)
import "base" Data.Functor.Compose (Compose (Compose))
#endif

-- Alt

deriving newtype instance (Bounded (f n)) => Bounded (Alt f n)

deriving newtype instance (Floating (f n)) => Floating (Alt f n)

deriving newtype instance (Fractional (f n)) => Fractional (Alt f n)

deriving newtype instance (Integral (f n)) => Integral (Alt f n)

deriving newtype instance (Real (f n)) => Real (Alt f n)

deriving newtype instance (RealFloat (f n)) => RealFloat (Alt f n)

deriving newtype instance (RealFrac (f n)) => RealFrac (Alt f n)

-- Ap

instance (Applicative f, Floating n) => Floating (Ap f n) where
  pi = pure pi
  exp = fmap exp
  log = fmap log
  sin = fmap sin
  cos = fmap cos
  asin = fmap asin
  acos = fmap acos
  atan = fmap atan
  sinh = fmap sinh
  cosh = fmap cosh
  asinh = fmap asinh
  acosh = fmap acosh
  atanh = fmap atanh

instance (Applicative f, Fractional n) => Fractional (Ap f n) where
  fromRational = pure . fromRational
  recip = fmap recip
  (/) = liftA2 (/)

-- Compose

#if !MIN_VERSION_base(4, 19, 0)
deriving newtype instance Bounded (f (g a)) => Bounded (Compose f g a)

deriving newtype instance Enum (f (g a)) => Enum (Compose f g a)

deriving newtype instance Integral (f (g a)) => Integral (Compose f g a)

deriving newtype instance Num (f (g a)) => Num (Compose f g a)

deriving newtype instance Real (f (g a)) => Real (Compose f g a)
#endif

#if !MIN_VERSION_base(4, 20, 0)
deriving newtype instance Floating (f (g a)) => Floating (Compose f g a)

deriving newtype instance Fractional (f (g a)) => Fractional (Compose f g a)

deriving newtype instance RealFloat (f (g a)) => RealFloat (Compose f g a)

deriving newtype instance RealFrac (f (g a)) => RealFrac (Compose f g a)
#endif

-- Double

maxRealFloat :: (RealFloat a) => a -> a
maxRealFloat a = encodeFloat m n
  where
    b = floatRadix a
    e = floatDigits a
    (_, e') = floatRange a
    m = b ^ e - 1
    n = e' - e

minRealFloat :: (RealFloat a) => a -> a
minRealFloat a = -(maxRealFloat a)

-- |
--
-- >>> minBound :: CDouble
-- -1.7976931348623157e308
-- >>> maxBound :: CDouble
-- 1.7976931348623157e308
instance Bounded CDouble where
  minBound = minRealFloat 0
  maxBound = maxRealFloat 0

-- |
--
-- >>> minBound :: Complex Double
-- (-1.7976931348623157e308) :+ (-1.7976931348623157e308)
-- >>> maxBound :: Complex Double
-- 1.7976931348623157e308 :+ 1.7976931348623157e308
instance (Bounded a) => Bounded (Complex a) where
  minBound = minBound :+ minBound
  maxBound = maxBound :+ maxBound

-- |
--
-- >>> minBound :: CFloat
-- -3.4028235e38
-- >>> maxBound :: CFloat
-- 3.4028235e38
instance Bounded CFloat where
  minBound = minRealFloat 0
  maxBound = maxRealFloat 0

-- |
--
-- >>> minBound :: Double
-- -1.7976931348623157e308
-- >>> maxBound :: Double
-- 1.7976931348623157e308
instance Bounded Double where
  minBound = minRealFloat 0
  maxBound = maxRealFloat 0

-- |
--
-- >>> minBound :: Float
-- -3.4028235e38
-- >>> maxBound :: Float
-- 3.4028235e38
instance Bounded Float where
  minBound = minRealFloat 0
  maxBound = maxRealFloat 0

-- Down

deriving newtype instance (Bounded n, Integral n) => Integral (Down n)

#if !MIN_VERSION_base(4, 18, 0)
instance (Enum a, Bounded a, Eq a) => Enum (Down a) where
  succ = fmap pred
  pred = fmap succ

  -- Here we use the fact that 'comparing (complement @Int)' behaves
  -- as an order-swapping `compare @Int`.
  fromEnum = complement . fromEnum . getDown
  toEnum = Down . toEnum . complement

  enumFrom (Down x)
      | x == minBound
      = [Down x] -- We can't rely on 'enumFromThen _ (pred @a minBound)` behaving nicely,
                 -- since 'enumFromThen _' might be strict and 'pred minBound' might throw
      | otherwise
      = coerce $ enumFromThen x (pred x)
  enumFromThen (Down x) (Down y) = coerce $ enumFromThen x y
#endif

-- Max

deriving newtype instance (Floating n) => Floating (Max n)

deriving newtype instance (Fractional n) => Fractional (Max n)

deriving newtype instance (Integral n) => Integral (Max n)

deriving newtype instance (Real n) => Real (Max n)

deriving newtype instance (RealFloat n) => RealFloat (Max n)

deriving newtype instance (RealFrac n) => RealFrac (Max n)

-- Min

deriving newtype instance (Floating n) => Floating (Min n)

deriving newtype instance (Fractional n) => Fractional (Min n)

deriving newtype instance (Integral n) => Integral (Min n)

deriving newtype instance (Real n) => Real (Min n)

deriving newtype instance (RealFloat n) => RealFloat (Min n)

deriving newtype instance (RealFrac n) => RealFrac (Min n)

-- Product

deriving newtype instance (Enum n) => Enum (Product n)

deriving newtype instance (Floating n) => Floating (Product n)

deriving newtype instance (Fractional n) => Fractional (Product n)

deriving newtype instance (Integral n) => Integral (Product n)

deriving newtype instance (Real n) => Real (Product n)

deriving newtype instance (RealFloat n) => RealFloat (Product n)

deriving newtype instance (RealFrac n) => RealFrac (Product n)

-- Sum

deriving newtype instance (Enum n) => Enum (Sum n)

deriving newtype instance (Floating n) => Floating (Sum n)

deriving newtype instance (Fractional n) => Fractional (Sum n)

deriving newtype instance (Integral n) => Integral (Sum n)

deriving newtype instance (Real n) => Real (Sum n)

deriving newtype instance (RealFloat n) => RealFloat (Sum n)

deriving newtype instance (RealFrac n) => RealFrac (Sum n)

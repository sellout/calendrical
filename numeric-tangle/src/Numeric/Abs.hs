{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-commercial
module Numeric.Abs
  ( Abs,
    Unsigned,
    abs,
  )
where

import "base" Control.Category ((.))
import "base" Data.Complex (Complex, magnitude)
import "base" Data.Fixed (Fixed (MkFixed), HasResolution, resolution)
import "base" Data.Function (($))
import "base" Data.Functor (Functor, fmap)
import "base" Data.Functor.Const (Const (Const))
import "base" Data.Functor.Contravariant (Op (Op))
import "base" Data.Functor.Identity (Identity (Identity))
import "base" Data.Int (Int, Int16, Int32, Int64, Int8)
import "base" Data.Kind (Constraint, Type)
import "base" Data.Monoid (Ap)
import "base" Data.Ratio (Ratio, denominator, numerator, (%))
import "base" Data.Word (Word, Word16, Word32, Word64, Word8)
import "base" Foreign.C.Types (CChar, CDouble, CFloat, CSChar, CUChar)
import "base" Numeric.Natural (Natural)
import "this" Numeric.Orphans ()
import "this" Numeric.Widen (widen)
import "base" Prelude (Double, Float, Integer, Integral, Real, fromIntegral)
import "base" Prelude qualified as Base (abs)

-- $setup
-- >>> import "base" Data.Ord ((<))
-- >>> import "base" Prelude (maxBound, minBound)

type Abs :: Type -> Constraint
class (Real (Unsigned n)) => Abs n where
  -- | This type serves multiple purposes. Absolute values are natural numbers,
  --   so we can often choose a more constrained type for the unsigned variant
  --   of an integer. However, there are also cases (e.g., `Complex` numbers and
  --   vectors where `abs` can overflow the underlying type, and so we need a
  --  /wider/ type to contain the result).
  type Unsigned n

  -- | The absolute value of @n@.
  abs :: n -> Unsigned n

-- |
--
-- >>> abs (minBound :: Int8)
-- 128
-- >>> abs (maxBound :: Int8)
-- 127
instance Abs Int8 where
  type Unsigned Int8 = Word8
  abs = fromIntegral . Base.abs

-- | @`magnitude` `maxBound` `maxBound`@ would overflow, so this needs to be
--   widened first.
--
-- >>> widen (maxBound :: CFloat) < abs (minBound :: Complex CFloat)
-- True
-- >>> widen (maxBound :: CFloat) < abs (maxBound :: Complex CFloat)
-- True
instance Abs (Complex CFloat) where
  type Unsigned (Complex CFloat) = CDouble
  abs = magnitude . widen

-- | @`magnitude` `maxBound` `maxBound`@ would overflow, so this needs to be
--   widened first.
--
-- >>> widen (maxBound :: Float) < abs (minBound :: Complex Float)
-- True
-- >>> widen (maxBound :: Float) < abs (maxBound :: Complex Float)
-- True
instance Abs (Complex Float) where
  type Unsigned (Complex Float) = Double
  abs = magnitude . widen

-- |
--
-- >>> abs (minBound :: CSChar)
-- 128
-- >>> abs (maxBound :: CSChar)
-- 127
instance Abs CSChar where
  type Unsigned CSChar = CUChar
  abs = fromIntegral . Base.abs

-- | `CChar` isn’t always signed, but we always define this instance to allow
--   for more portable code.
instance Abs CChar where
  type Unsigned CChar = CUChar
  abs = fromIntegral . Base.abs

instance Abs Int16 where
  type Unsigned Int16 = Word16
  abs = fromIntegral . Base.abs

instance Abs Int32 where
  type Unsigned Int32 = Word32
  abs = fromIntegral . Base.abs

instance Abs Int64 where
  type Unsigned Int64 = Word64
  abs = fromIntegral . Base.abs

instance Abs Int where
  type Unsigned Int = Word
  abs = fromIntegral . Base.abs

instance Abs Integer where
  type Unsigned Integer = Natural
  abs = fromIntegral . Base.abs

instance (Abs s, Integral (Unsigned s)) => Abs (Ratio s) where
  type Unsigned (Ratio s) = Ratio (Unsigned s)
  abs s = abs (numerator s) % abs (denominator s)

instance (HasResolution n) => Abs (Fixed n) where
  type Unsigned (Fixed n) = Ratio Natural
  abs a@(MkFixed s) = abs s % abs (resolution a)

instance (Functor f, Abs s, Real (Ap f (Unsigned s))) => Abs (Ap f s) where
  type Unsigned (Ap f s) = Ap f (Unsigned s)
  abs = fmap abs

instance (Abs s) => Abs (Const s a) where
  type Unsigned (Const s a) = Unsigned s
  abs (Const s) = abs s

instance (Abs s) => Abs (Identity s) where
  type Unsigned (Identity s) = Unsigned s
  abs (Identity a) = abs a

instance (Abs s, Real (Op (Unsigned s) a)) => Abs (Op s a) where
  type Unsigned (Op s a) = Op (Unsigned s) a
  abs (Op fn) = Op $ abs . fn

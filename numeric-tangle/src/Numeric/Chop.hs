{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeFamilies #-}
-- for a @`Bounded` (`Chopped` a)@ constraint
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-commercial
module Numeric.Chop
  ( Chop,
    Chopped,
    ceiling,
    floor,
    mixedFraction,
    properFraction,
    round,
    truncate,
  )
where

import "base" Control.Applicative (pure)
import "base" Control.Category ((.))
import "base" Data.Bifunctor (bimap)
import "base" Data.Fixed (Fixed, HasResolution)
import "base" Data.Functor (fmap)
import "base" Data.Functor.Compose (Compose (Compose), getCompose)
import "base" Data.Functor.Const (Const (Const), getConst)
-- import "base" Data.Functor.Contravariant (Op (Op))
import "base" Data.Functor.Identity (Identity, runIdentity)
import "base" Data.Kind (Constraint, Type)
import "base" Data.Monoid (Alt (Alt), Product, Sum, getAlt, getProduct, getSum)
import "base" Data.Ord (Down, Ordering (EQ, GT, LT), compare, getDown, (<))
import "base" Data.Ratio (Ratio)
import "base" Data.Semigroup (Max, Min, getMax, getMin)
import "base" Data.Tuple (fst, snd)
import "base" Foreign.C.Types (CDouble, CFloat)
import "this" Numeric.Orphans ()
import "base" Prelude
  ( Bounded,
    Double,
    Float,
    Fractional,
    Integer,
    Integral,
    Real,
    abs,
    even,
    (+),
    (-),
  )
import "base" Prelude qualified as Base
  ( ceiling,
    floor,
    properFraction,
    round,
    truncate,
  )

-- | Extracting components of fractions.
--
--  __NB__: This is exactly `Prelude.RealFrac` with a more constrained result
--          type. Also, `Base.properFraction` is called `mixedFraction` here,
--          with `properFraction` being a different function.
type Chop :: Type -> Constraint
class (Fractional a, Real a, Integral (Chopped a)) => Chop a where
  {-# MINIMAL mixedFraction | truncate, properFraction #-}

  -- | This type must be able to represent all integers in the range
  --   [⌊@`minBound` :: a@⌋, ⌈@`maxBound` :: a`⌉]
  type Chopped a

  -- | The function `mixedFraction` takes a real fractional number @x@ and
  --   returns a pair @(n,f)@ such that @x = n+f@, and:
  --
  -- * @n@ is an integral number with the same sign as @x@; and
  --
  -- * @f@ is a fraction with the same type and sign as @x@, and with absolute
  --   value less than @1@.
  --
  --   The default definitions of the `ceiling`, `floor`, `truncate`, and
  --  `round` functions are in terms of `mixedFraction`.
  mixedFraction :: a -> (Chopped a, a)
  mixedFraction x = (truncate x, properFraction x)

  -- | @`ceiling` x@ returns the least integer not less than @x@
  ceiling :: a -> Chopped a
  ceiling x =
    let (n, r) = mixedFraction x
     in if 0 < r then n + 1 else n

  -- | @`floor` x@ returns the greatest integer not greater than @x@
  floor :: a -> Chopped a
  floor x =
    let (n, r) = mixedFraction x
     in if r < 0 then n - 1 else n

  -- | @`round` x@ returns the nearest integer to @x@;
  --   the even integer if @x@ is equidistant between two integers
  round :: a -> Chopped a
  round x =
    let (n, r) = mixedFraction x
        m = if r < 0 then n - 1 else n + 1
     in case compare (abs r - 0.5) 0 of
          LT -> n
          EQ -> if even n then n else m
          GT -> m

  -- | @`truncate` x@ returns the integer nearest @x@ between zero and @x@.
  truncate :: a -> Chopped a
  truncate = fst . mixedFraction
  {-# INLINE truncate #-}

  -- | @`properFraction` x@, unlike `Base.properFraction`, returns @x -
  --  `truncate` x@ – the proper fraction that remains when the integral
  --   component is removed.
  properFraction :: a -> a
  properFraction = snd . mixedFraction

instance (Integral a) => Chop (Ratio a) where
  type Chopped (Ratio a) = a
  ceiling = Base.ceiling
  floor = Base.floor
  mixedFraction = Base.properFraction
  round = Base.round
  truncate = Base.truncate

instance Chop Float where
  type Chopped Float = Integer
  ceiling = Base.ceiling
  floor = Base.floor
  mixedFraction = Base.properFraction
  round = Base.round
  truncate = Base.truncate

instance Chop Double where
  type Chopped Double = Integer
  ceiling = Base.ceiling
  floor = Base.floor
  mixedFraction = Base.properFraction
  round = Base.round
  truncate = Base.truncate

instance Chop CFloat where
  type Chopped CFloat = Integer
  ceiling = Base.ceiling
  floor = Base.floor
  mixedFraction = Base.properFraction
  round = Base.round
  truncate = Base.truncate

instance Chop CDouble where
  type Chopped CDouble = Integer
  ceiling = Base.ceiling
  floor = Base.floor
  mixedFraction = Base.properFraction
  round = Base.round
  truncate = Base.truncate

instance (HasResolution n) => Chop (Fixed n) where
  type Chopped (Fixed n) = Integer
  ceiling = Base.ceiling
  floor = Base.floor
  mixedFraction = Base.properFraction
  round = Base.round
  truncate = Base.truncate

instance (Chop (f a)) => Chop (Alt f a) where
  type Chopped (Alt f a) = Chopped (f a)
  ceiling = ceiling . getAlt
  floor = floor . getAlt
  mixedFraction = fmap Alt . mixedFraction . getAlt
  round = round . getAlt
  truncate = truncate . getAlt

-- Need @`Comonad` f@.
-- instance (Functor f, Chop a) => Chop (Ap f a) where
--   type Chopped (Ap f a) = Ap f (Chopped a)
--   ceiling = fmap ceiling
--   floor = fmap floor
--   mixedFraction = unzip . fmap mixedFraction
--   round = fmap round
--   truncate = fmap truncate

instance (Chop (f (g a))) => Chop (Compose f g a) where
  type Chopped (Compose f g a) = Chopped (f (g a))
  ceiling = ceiling . getCompose
  floor = floor . getCompose
  mixedFraction = fmap Compose . mixedFraction . getCompose
  round = round . getCompose
  truncate = truncate . getCompose

-- |
--
--  __NB__: This instance doesn’t use the @`Bifunctor` `Const`@ instance,
--          because it requires @a :: `Type`@.
instance (Chop a) => Chop (Const a b) where
  type Chopped (Const a b) = Const (Chopped a) b
  ceiling = Const . ceiling . getConst
  floor = Const . floor . getConst
  mixedFraction = bimap Const Const . mixedFraction . getConst
  round = Const . round . getConst
  truncate = Const . truncate . getConst

instance (Chop a, Bounded (Chopped a)) => Chop (Down a) where
  type Chopped (Down a) = Down (Chopped a)
  ceiling = fmap ceiling
  floor = fmap floor
  mixedFraction = bimap pure pure . mixedFraction . getDown
  round = fmap round
  truncate = fmap truncate

instance (Chop a) => Chop (Identity a) where
  type Chopped (Identity a) = Identity (Chopped a)
  ceiling = fmap ceiling
  floor = fmap floor
  mixedFraction = bimap pure pure . mixedFraction . runIdentity
  round = fmap round
  truncate = fmap truncate

instance (Chop a) => Chop (Max a) where
  type Chopped (Max a) = Max (Chopped a)
  ceiling = fmap ceiling
  floor = fmap floor
  mixedFraction = bimap pure pure . mixedFraction . getMax
  round = fmap round
  truncate = fmap truncate

instance (Chop a) => Chop (Min a) where
  type Chopped (Min a) = Min (Chopped a)
  ceiling = fmap ceiling
  floor = fmap floor
  mixedFraction = bimap pure pure . mixedFraction . getMin
  round = fmap round
  truncate = fmap truncate

-- instance (Chop a) => Chop (Op a b) where
--   type Chopped (Op a b) = Op (Chopped a) b
--   ceiling = Op . fmap ceiling . getOp
--   floor = Op . fmap floor . getOp
--   mixedFraction = unzip . Op . fmap mixedFraction . getOp
--   round = Op . fmap round . getOp
--   truncate = Op . fmap truncate . getOp

instance (Chop a) => Chop (Product a) where
  type Chopped (Product a) = Product (Chopped a)
  ceiling = fmap ceiling
  floor = fmap floor
  mixedFraction = bimap pure pure . mixedFraction . getProduct
  round = fmap round
  truncate = fmap truncate

instance (Chop a) => Chop (Sum a) where
  type Chopped (Sum a) = Sum (Chopped a)
  ceiling = fmap ceiling
  floor = fmap floor
  mixedFraction = bimap pure pure . mixedFraction . getSum
  round = fmap round
  truncate = fmap truncate

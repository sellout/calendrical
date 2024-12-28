{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fplugin-opt=NoRecursion:ignore-decls:next #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-commercial
--
-- This module provides some general types so that we can avoid importing the
-- underlying types in other modules, to encourage consistent usage.
module Data.Calendar.Types
  ( Amplitude,
    Angle,
    AngleT,
    Boolean,
    Degree,
    Duration,
    Integer,
    Minute,
    Mod,
    ModularEnum,
    NonegativeReal,
    NonnegativeInteger,
    NonzeroInteger,
    NonzeroReal,
    Phase,
    PositiveInteger,
    PositiveReal,
    Radian,
    Rational,
    RationalAmplitude,
    RationalAngle,
    Real,
    Season,
    amod,
    angle,
    arcsinDegrees,
    arctanDegrees,
    binarySearch,
    bogus,
    cosDegrees,
    deg,
    degreesFromRadians,
    degreesMinutesSeconds,
    false,
    final,
    hr,
    mins,
    mn,
    mod,
    mod1,
    modI,
    modularToEnum,
    next,
    poly,
    prod,
    quotient,
    radiansFromDegrees,
    sec,
    secs,
    sigma,
    sign,
    sinDegrees,
    sum,
    tanDegrees,
    true,
    (%),
  )
where

import "base" Control.Applicative (pure)
import "base" Control.Category (id, (.))
import "base" Data.Bool (Bool (False, True), (&&))
import "base" Data.Either (Either (Left), either)
import "base" Data.Eq (Eq, (==))
import "base" Data.Foldable (foldMap, foldlM, foldr)
import "base" Data.Function (const, ($))
import "base" Data.Int qualified as Base (Int)
import "base" Data.Kind (Constraint, Type)
import "base" Data.Maybe (Maybe (Just, Nothing))
import "base" Data.Monoid (Sum (Sum), getSum)
import "base" Data.Ord ((<=))
import "base" Data.Ratio (Ratio, Rational)
import "base" Data.Tuple (fst)
import "base" Numeric (asin, atan, cos, pi, sin, tan)
import "base" Numeric.Natural (Natural)
import "fin" Data.Fin (Fin)
import "fin" Data.Type.Nat (Nat)
import "fin" Data.Type.Nat qualified as Nat
import "mixed-radix" Numeric.MixedRadix
  ( MixedIntegral (Unbounded),
    MixedRadix (FinalFrac, FracRadix, Integral),
  )
import "numeric-tangle" Numeric.Chop (ceiling, floor, mixedFraction)
import "numeric-tangle" Numeric.Ration (rationalize, (%))
import "numeric-tangle" Numeric.Widen (widen)
import "base" Prelude
  ( Bounded,
    Enum,
    Integer,
    Num,
    error,
    fromEnum,
    fromIntegral,
    fromRational,
    maxBound,
    minBound,
    signum,
    succ,
    (*),
    (+),
    (-),
    (/),
  )
import "base" Prelude qualified as Base (Double, mod)

type Boolean :: Type
type Boolean = Bool

-- | This is a tough one. @`Ratio` `Natural`@ is not a subset of `Double`, but
--   it does exclude the negatives. Eventually, all the math might be rational,
--   in which case this makes more sense, but keep an eye on this one.
--
--  __NB__: The book’s code typos this as “nonegative-real” (missing the
--          doubled-“n”).
type NonegativeReal :: Type
type NonegativeReal = Ratio Natural

type NonnegativeInteger :: Type
type NonnegativeInteger = Natural

-- |
--
--  __TODO__: See if this is still used after fixing the off-by-one issues.
type NonzeroInteger :: Type
type NonzeroInteger = Integer

-- |
--
--  __TODO__: See if this is still used after fixing the off-by-one issues.
type NonzeroReal :: Type
type NonzeroReal = Real

-- |
--
--  __TODO__: This is silghtly too loose. My guess is that most (if not all)
--            cases will become `NonNegativeInteger` once we fix the general
--            off-by-oneness of the book, so don’t try fixing this (e.g., with a
--            @newtype@ over `Natural` that shifts by one) until that other
--            problem is solved.
type PositiveInteger :: Type
type PositiveInteger = Natural

-- |
--
--  __TODO__: See if this is still used after fixing the off-by-one issues.
type PositiveReal :: Type
type PositiveReal = NonegativeReal

-- | This is poorly named, because it conflicts with the type class, but it’s
--   what the book uses. And we use this synonym rather than `Double` directly
--   so that we can swap it out with other types (like `Rational`) easily.
type Real :: Type
type Real = Rational -- Double

-- | A modular enumeration loops around its values, so that `toEnum` is total
--  (but may return the same enumeration value for multiple `Int`s). It
--   additionally requires that the enumeration values have contiguous `Int`
--   values. It’s also strongly recommended that instances have fewer values
--   than `Int`, so that `fromEnum` can be total.
type ModularEnum :: Type -> Constraint
class (Enum a, Bounded a) => ModularEnum a

modularToEnum :: forall a. (ModularEnum a) => Base.Int -> a
modularToEnum i =
  let minb = fromEnum @a minBound
   in foldr
        (const succ)
        minBound
        [1 .. (i - minb) `mod` (fromEnum @a maxBound - minb)]

-- | Generalizes `Prelude.mod` to handle non-`Integral` types.
--
--  __TODO__: This can probably be eliminated by using more type-specific
--            operations in the appropriate places. E.g., `mod realFracValue 1`
--            is better as `snd $ mixedFraction realFracValue`.
type Mod :: Type -> Constraint
class (Num a) => Mod a where
  mod :: a -> a -> a

instance Mod Base.Int where
  mod = Base.mod

instance Mod Integer where
  mod = Base.mod

instance Mod NonnegativeInteger where
  mod = Base.mod

instance Mod Real where
  mod x y = x - y * rationalize (floor $ x % y)

true :: Boolean
true = True

false :: Boolean
false = False

bogus :: a
bogus = error "bogus"

-- | Whole part of @m@/@n@.
quotient :: Real -> NonzeroReal -> Integer
quotient m = fst . mixedFraction . (m %)

-- | The value of (@x@ mod @y@) with @y@ instead of 0.
amod :: Integer -> NonzeroInteger -> Integer
amod x y = y + x `mod` (-y)

-- instance Mod Natural where
--   mod = Prelude.mod

modI :: (Eq a, Mod a) => a -> (a, a) -> a
modI x (a, b) = if a == b then x else a + (x - a `mod` b - a)

mod1 :: (Eq a, Mod a) => a -> a -> a
mod1 x b = let r = x `mod` b in if r == 0 then b else r

-- | [0..360)
type Angle :: Type
type Angle = Real

type RationalAngle :: Type
type RationalAngle = Angle

angle :: Integer -> Integer -> Real -> Angle
angle d m s = fromIntegral d + (fromIntegral m + s / 60) / 60

deg :: Real -> Angle
deg x = x

mins :: Real -> Angle
mins = (/ 60)

secs :: Real -> Angle
secs = (/ 3600)

type Duration :: Type
type Duration = Real

hr :: Real -> Duration
hr = (/ 24)

mn :: Real -> Duration
mn = (/ (24 * 60))

sec :: Real -> Duration
sec = (/ (24 * 60 * 60))

type MinuteBound :: Nat
type MinuteBound = Nat.FromGHC 60

type SecondBound :: Nat
type SecondBound = Nat.FromGHC 60

-- |
--
--  __NB__: The more common `Angle` is real-valued, but the book also refers to
--          this type as @angle@.
--
--  __NB__: Not sure if this will get used – radians generally makes much more
--          sense.
type AngleT :: Type
type AngleT = MixedRadix '[] 'False '[MinuteBound, SecondBound] ('Just Real)

-- |
--
--  __NB__: This is used as a “type” in the code, but it is not included in the
--          list of types (Appendix A.1).
type Degree :: Type
type Degree = Integer

-- |
--
--  __NB__: This is used as a “type” in the code, but it is not included in the
--          list of types (Appendix A.1).
type Minute :: Type
type Minute = Fin MinuteBound

degreesMinutesSeconds :: Degree -> Minute -> Real -> AngleT
degreesMinutesSeconds d m = Integral (Unbounded d) . FracRadix m . FinalFrac

-- -- |
-- --
-- --  __NB__: The book represents a negative angle with negatives in all three
-- --          positions, but `AngleT` uses `MixedRadix`, which stores a normalized
-- --          format with only the most significant component (degrees) being
-- --          signed.
-- angleFromDegrees :: Angle -> AngleT
-- angleFromDegrees = _

type Radian :: Type
type Radian = Real

-- | In the range [-1..1).
type Amplitude :: Type
type Amplitude = Real

type RationalAmplitude :: Type
type RationalAmplitude = Amplitude

degreesFromRadians :: Radian -> Angle
degreesFromRadians theta = theta / widen @Base.Double pi * 180 `mod` 360

-- |
--
--  __TODO__: The input type should probably be `Angle`, but the book says
--            @real@.
radiansFromDegrees :: Real -> Radian
radiansFromDegrees theta = (theta `mod` 360) * widen @Base.Double pi / 180

sinDegrees :: Angle -> Amplitude
sinDegrees = widen . sin @Base.Double . fromRational . radiansFromDegrees

cosDegrees :: Angle -> Amplitude
cosDegrees = widen . cos @Base.Double . fromRational . radiansFromDegrees

tanDegrees :: Angle -> Real
tanDegrees = widen . tan @Base.Double . fromRational . radiansFromDegrees

arcsinDegrees :: Amplitude -> Angle
arcsinDegrees = degreesFromRadians . widen . asin @Base.Double . fromRational

-- | Arctangent of @y/x@ in degrees. Returns bogus if @x@ and @y@ are both 0.
arctanDegrees :: Real -> Real -> Angle
arctanDegrees y x =
  if x == 0 && y == 0
    then bogus
    else
      ( if x == 0
          then signum y * deg 90
          else
            let alpha =
                  degreesFromRadians
                    . widen
                    . atan @Base.Double
                    . fromRational
                    $ y
                      / x
             in if 0 <= x then alpha else alpha + deg 180
      )
        `mod` 360

-- | First integer greater or equal to @initial@ such that @condition@ holds.
next :: Integer -> (Integer -> Bool) -> Integer
next initial condition =
  if condition initial then initial else next (succ initial) condition

-- | Last integer greater or equal to @initial@ such that @condition@ holds.
--
--  __NB__: The original description isn’t quite true. The function actually
--          returns the integer before the first integer that fails the
--          condition. But for this library, those are probably identical, as
--          it’s generally just about bounds. This implementation also returns
--          `Nothing` if @initial@ fails the condition, where the CL impl
--          returns @initial - 1@ in that case.
final :: Integer -> (Integer -> Bool) -> Maybe Integer
final initial condition =
  either id id $
    foldlM
      (\prev index -> if condition index then pure $ pure index else Left prev)
      Nothing
      [initial ..]

-- | Sum @expression@ for @index = initial@ and successive integers, as long as
--   @condition@ holds.
sum :: (Integer -> Real) -> Integer -> (Integer -> Bool) -> Real
sum expression initial condition =
  either id id $
    foldlM
      ( \acc index ->
          if condition index then pure $ acc + expression index else Left acc
      )
      0
      [initial ..]

-- | Product of @expression@ for @index = initial@ and successive integers, as
--   long as @condition@ holds.
prod :: (Integer -> Real) -> Integer -> (Integer -> Bool) -> Real
prod expression initial condition =
  either id id $
    foldlM
      ( \acc index ->
          if condition index then pure $ acc * expression index else Left acc
      )
      1
      [initial ..]

-- | Bisection search for a value in @[lo..hi]@ such that @end@ holds.  @test@
--   determines when to go left.
--
--  __NB__: The original can fail to terminate, but this version will eventually
--          return `Nothing` in that case.
binarySearch ::
  Real -> Real -> (Real -> Bool) -> (Real -> Real -> Boolean) -> Maybe Real
binarySearch lo hi test end =
  -- __TODO__: This is fine because it’s guaranteed not to run more than ⌈hi -
  --           lo⌉ times, but the algorithm is O(log(⌈hi - lo⌉)), and we should
  --           be able to guard that with a better `cata`.
  either pure (const Nothing) $
    foldlM
      ( \(l, h) (_ :: Integer) ->
          let x = (h + l) / 2
              left = test x
              l' = if left then l else x
              h' = if left then x else h
           in if end l' h' then Left ((h' + l') / 2) else pure (l', h')
      )
      (lo, hi)
      [floor lo .. ceiling hi]

-- | @list@ is of the form ((i1 l1)...(in ln)). Sum of @body@ for indices
--   i1...in running simultaneously thru lists l1...ln.
--
--  __NB__: For this library @a@ is a tuple of arbitrary size.
sigma :: (Num b) => [a] -> (a -> b) -> b
sigma list fn = getSum $ foldMap (Sum . fn) list

-- | Sum powers of @x@ with coefficients (from order 0 up) in list @a@.
poly :: Real -> [Real] -> Real
poly x = foldr (\a acc -> a + x * acc) 0

-- | Sign of @y@.
sign ::
  Real ->
  -- | {-1,0,+1}
  Real
sign = signum

type Phase :: Type
type Phase = Angle

type Season :: Type
type Season = Angle

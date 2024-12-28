{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-name-shadowing -Wno-unused-top-binds #-}
-- __FIXME__: I think this is due to a GHC bug.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -fplugin-opt=NoRecursion:ignore-decls:positionsInRange #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-commercial
module Data.Calendar
  ( Calendar,
    CyclicCalendar,
    DayOfWeek (Friday, Monday, Saturday, Sunday, Thursday, Tuesday, Wednesday),
    Interval,
    JulianDayNumber (JD),
    FixedDate (RD),
    Moment (Moment),
    Range,
    RationalMoment,
    StandardDay,
    StandardMonth,
    StandardYear,
    clockFromMoment,
    epoch,
    fixedFrom,
    fixedsFrom,
    fromFixed,
    fromMoment,
    intervalClosed,
    invertAngular,
    kdayAfter,
    kdayBefore,
    kdayNearest,
    kdayOnOrAfter,
    kdayOnOrBefore,
    listRange,
    momentFrom,
    momentToReal,
    offset,
    positionsInRange,
    rd,
    timeFromClock,
    timeFromMoment,
    validDate,

    -- * mod
    Mod (..),
    modI,
    mod1,
  )
where

import "base" Control.Applicative (pure)
import "base" Control.Category ((.))
import "base" Control.Monad ((=<<))
import "base" Data.Bifunctor (bimap)
import "base" Data.Bool (Bool (False, True), (&&))
import "base" Data.Either (Either)
import "base" Data.Eq (Eq, (==))
import "base" Data.Foldable (foldr)
import "base" Data.Function (($))
import "base" Data.Functor (fmap)
import "base" Data.Functor.Identity (Identity (Identity))
import "base" Data.Kind (Constraint, Type)
import "base" Data.List.NonEmpty (NonEmpty ((:|)))
import "base" Data.Maybe (Maybe (Just))
import "base" Data.Ord (Ord, (<), (<=))
import "base" Data.Proxy (Proxy (Proxy))
import "base" Data.Tuple (fst, snd, uncurry)
import "base" Numeric.Natural (Natural)
import "fin" Data.Fin (Fin)
import "fin" Data.Type.Nat qualified as Nat
import "mixed-radix" Numeric.MixedRadix (MixedRadix)
import "mixed-radix" Numeric.MixedRadix qualified as MixedRadix
import "numeric-tangle" Numeric.Chop (floor, mixedFraction)
import "numeric-tangle" Numeric.Ration (rationalize, (%))
import "numeric-tangle" Numeric.Widen (widen)
import "this" Data.Calendar.Types
  ( Angle,
    Integer,
    Mod,
    ModularEnum,
    NonegativeReal,
    PositiveReal,
    Real,
    binarySearch,
    deg,
    mod,
    mod1,
    modI,
    modularToEnum,
  )
import "base" Prelude
  ( Bounded,
    Enum, -- __TODO__: Replace `Enum` with `Prism'`.
    Num,
    fromEnum,
    -- __TODO__: Remove, because it‘s only used in lossy cases. And we also want
    --           a “weaker” `Enum` that gives us `prev`/`succ` without the
    --           mapping to `Int`.
    fromIntegral,
    toEnum,
    (*),
    (+),
    (-),
  )

type FixedDate :: Type
newtype FixedDate = RD {offset :: Integer}
  deriving newtype (Enum, Eq, Num, Ord)

fixedFromInteger :: Integer -> FixedDate -> FixedDate
fixedFromInteger n epoch = RD n + epoch

dayNumberFromFixed :: FixedDate -> FixedDate -> Integer
dayNumberFromFixed (RD date) (RD epoch) = date - epoch

-- | A `FixedDate` that has a fractional part is called a “moment”.
type Moment :: Type
newtype Moment = Moment {momentToReal :: Real}
  deriving newtype (Eq, Mod, Num, Ord)

type RationalMoment :: Type
type RationalMoment = Moment

-- |
--
--  __FIXME__: Not part of this library
--
--  __FIXME__: The day radix doesn’t really work here, as it changes
--             month-by-month (and on leap years).
type IsoDateTime :: Maybe Type -> Type
type IsoDateTime =
  MixedRadix
    '[Nat.FromGHC 31 {- day of month -}, Nat.FromGHC 12 {- month -}]
    'False -- year
    '[ Nat.FromGHC 24, -- hour
       Nat.FromGHC 60, -- minute
       Nat.FromGHC 60 -- second
     ]

-- | A `CyclicCalendar` doesn’t assign _unique_ names to dates, but can map
--   multiple dates to the same name. A simple example is `DayOfWeek`, which
--   maps all dates to one of @`Sunday` .. `Saturday`@.
--
-- -prop> let date = fromFixed rd' in rd' `elem` fixedsFrom date
type CyclicCalendar :: Type -> Constraint
class CyclicCalendar date where
  -- | Even though cyclic calendars don’t necessarily have an “epoch” as such,
  --   this always exists as a starting point for calculating the cycles. If
  --   there isn’t a particularly significant value, then the cycle start
  --   closest to @`RD` 0@ is a good choice.
  --
  --  __TODO__: Determine if it makes sense for calendars to always extend
  --            infinitely. E.g., maybe it’s non-sensical to talk about
  --           `Olympiad`s before the first one, so negative values just don’t
  --            exist. Or maybe a calendar ends at some point (like people
  --            mistakenly thought about the `Mayan` calendar), and so dates
  --            beyond that pon’t don’t exist. In either of these cases
  --           `fixedsFrom` would return a non-infinite result.
  epoch :: proxy date -> FixedDate

  fromFixed :: FixedDate -> date
  fromFixed = fromMoment . momentFrom

  -- | This returns either exactly one or an infinite number of `FixedDate`s. The
  --   distinction is whether @date@ also implements `Calendar`, in which case
  --   this must return a single value.
  --
  --   This returns values ordered by distance from @`RD` 0@. (Or is it distance
  --   from @`epoch` (`Proxy` :: `Proxy` date)@?)
  fixedsFrom :: date -> NonEmpty FixedDate
  default fixedsFrom :: (Calendar date) => date -> NonEmpty FixedDate
  fixedsFrom = pure . fixedFrom

  fromMoment :: Moment -> date

-- |
--
--  `fixedFrom` must be an `Ord` homomorphism.
--   -prop> compare date1 date2 == compare (fixedFrom date1) (fixedFrom date2)
--
--  __TODO__: Figure out the class hierarchy. E.g., there are RD and moment
--            calendars, and some calendars are “cyclic”, where an RD becomes a
--            single “date”, but a “date” is a possibly infinite list of RDs (or
--            moments).
type Calendar :: Type -> Constraint
class (CyclicCalendar date, Ord date) => Calendar date where
  -- | I thisk these two form an isomorphism
  fixedFrom :: date -> FixedDate
  fixedFrom = fixedFromMoment . momentFrom

  -- | These two are only guaranteed to form an adjunction (moment -> _ may lose
  --   precision, so _ -> moment is <=).
  momentFrom :: date -> Moment
  momentFrom = momentFrom . fixedFrom

validDate :: (Calendar date) => date -> Bool
validDate date = date == fromFixed (fixedFrom date)

rd :: (Calendar date) => proxy date -> FixedDate -> Integer
rd proxy d = offset d - offset (epoch proxy)

instance CyclicCalendar FixedDate where
  epoch _ = RD 0
  fromFixed date = RD . dayNumberFromFixed date . epoch $ Identity date
  fromMoment = fixedFromMoment

instance Calendar FixedDate where
  fixedFrom date = fixedFromInteger (offset date) . epoch $ Identity date
  momentFrom = Moment . rationalize . offset

type JulianDayNumber :: Type
newtype JulianDayNumber = JD {dayNumber :: Real}
  deriving stock (Eq, Ord)

instance CyclicCalendar JulianDayNumber where
  -- (1.3)
  epoch _ = RD -1721424 -- __FIXME__: Should be @-1721424.5@.
  -- (1.5)

  fromMoment (Moment t) =
    JD $ t - rationalize (offset $ epoch (Proxy :: Proxy JulianDayNumber))

instance Calendar JulianDayNumber where
  -- (1.4)
  momentFrom jd =
    Moment $ dayNumber jd + rationalize (offset . epoch $ Identity jd)

jd :: (Calendar date) => Integer -> date
jd n = fromFixed . RD $ n - 1721425

-- | (1.12)
fixedFromMoment :: Moment -> FixedDate
fixedFromMoment (Moment t) = RD $ floor t

timeFromMoment :: Moment -> Time
timeFromMoment (Moment t) = snd $ mixedFraction t

-- | (1.37)
--
--  __TODO__: Delete this – it’s just an `fmap`.
listOfFixedFromMoments :: [Moment] -> [FixedDate]
listOfFixedFromMoments = fmap fixedFromMoment

type Interval :: Type
type Interval = (Moment, Moment)

-- | Use bisection to find inverse of angular function @f@ at @y@ within
--   interval @r@.
invertAngular :: (Real -> Angle) -> Real -> Interval -> Maybe Real
invertAngular f y r =
  uncurry
    binarySearch
    -- __TODO__: Shouldn’t need `momentToReal` here – `Interval` should probably
    --           be defined on `Real`, or even more generically.
    (bimap momentToReal momentToReal r)
    (\x -> (f x - y) `mod` 360 < deg 180)
    $ \l u -> u - l < widen ε
  where
    ε = 1 % (100000 :: Natural) -- desired accuracy

interval :: Moment -> Moment -> Interval
interval = (,)

-- |
--
--  __TODO__: Create a separate type for closed intervals.
intervalClosed :: Moment -> Moment -> Interval
intervalClosed = (,)

begin :: Interval -> Moment
begin = fst

end :: Interval -> Moment
end = snd

isInRange :: Moment -> Interval -> Bool
isInRange tee range = begin range <= tee && tee < end range

listRange :: [Moment] -> Interval -> [Moment]
listRange ell range =
  foldr (\h r -> if isInRange h range then h : r else r) [] ell

-- | List of occurrences of moment @p@ of @c@-day cycle within @range@.
--
--   @δ@ is position in cycle of RD moment 0.
--
--   (1.40)
positionsInRange ::
  NonegativeReal -> PositiveReal -> NonegativeReal -> Interval -> [Moment]
positionsInRange p c δ (a, b) =
  if b <= date
    then []
    else date : positionsInRange p c δ (interval next b)
  where
    next = a + Moment (widen c)
    date = Moment (widen $ p - δ) `modI` interval a next

type Range :: Type
type Range = (FixedDate, FixedDate)

type Hour :: Type
type Hour = Fin (Nat.FromGHC 24)

type Second :: Type
type Second = Real

type ClockTime :: Type
type ClockTime =
  MixedRadix
    '[]
    'True
    '[Nat.FromGHC 24, Nat.FromGHC 60, Nat.FromGHC 60]
    ('Just Second)

type Time :: Type
type Time = Real

timeFromClock :: ClockTime -> Time
timeFromClock = (widen (1 % (24 :: Natural)) *) . MixedRadix.eval

clockFromMoment :: Moment -> Either MixedRadix.FloatFailure ClockTime
clockFromMoment (Moment t) = MixedRadix.interpret . snd $ mixedFraction t

-- instance CyclicCalendar Hebrew where
--   epoch _ = RD -1373427

-- instance CyclicCalendar Mayan where
--   epoch _ = RD -1137142

-- instance CyclicCalendar Chinese where
--   epoch _ = RD -963099

-- instance CyclicCalendar Samaritan where
--   epoch _ = RD -598573

-- instance CyclicCalendar Babylonian where
--   epoch _ = RD -113502

-- instance CyclicCalendar Tibetan where
--   epoch _ = RD -46410

-- instance CyclicCalendar ISO where
--   epoch _ = RD 1

-- instance CyclicCalendar Persian where
--   epoch _ = RD 226896

-- instance CyclicCalendar Islamic where
--   epoch _ = RD 227015

type DayOfWeek :: Type
data DayOfWeek
  = Sunday
  | Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  deriving stock (Bounded, Eq, Ord)

instance Enum DayOfWeek where
  fromEnum = \case
    Sunday -> 0
    Monday -> 1
    Tuesday -> 2
    Wednesday -> 3
    Thursday -> 4
    Friday -> 5
    Saturday -> 6
  toEnum = modularToEnum

instance ModularEnum DayOfWeek

instance CyclicCalendar DayOfWeek where
  epoch _ = RD 0

  -- (1.60)
  fromFixed date =
    toEnum $
      fromIntegral (offset $ date - epoch (Proxy :: Proxy DayOfWeek))
        - fromEnum Sunday

  fixedsFrom dow =
    let origin = RD . widen $ fromEnum dow `mod` 7
     in origin
          :| ( ( \week ->
                   let days = 7 * week in [origin - days, origin + days]
               )
                 -- __TODO__: Use a Yaya unfold for this, to avoid the partial
                 --          `Enum` instance on `FixedDate`.
                 =<< [1 ..]
             )
  fromMoment (Moment t) = fromFixed . RD $ floor t

type StandardMonth :: Type
type StandardMonth = Fin (Nat.FromGHC 14)

type StandardDay :: Type
type StandardDay = Fin (Nat.FromGHC 32)

type StandardYear :: Type
type StandardYear = Integer

kdayOnOrBefore :: DayOfWeek -> FixedDate -> FixedDate
kdayOnOrBefore k (RD date) =
  RD $
    date
      - widen
        (fromEnum @DayOfWeek . fromFixed . RD $ date - widen (fromEnum k))

kdayOnOrAfter :: DayOfWeek -> FixedDate -> FixedDate
kdayOnOrAfter k = kdayOnOrBefore k . (+ 6)

kdayNearest :: DayOfWeek -> FixedDate -> FixedDate
kdayNearest k = kdayOnOrBefore k . (+ 3)

kdayBefore :: DayOfWeek -> FixedDate -> FixedDate
kdayBefore k = kdayOnOrBefore k . (\n -> n - 1)

kdayAfter :: DayOfWeek -> FixedDate -> FixedDate
kdayAfter k = kdayOnOrBefore k . (+ 7)

-- instance CyclicCalendar FrenchRevolutionary where
--   epoch _ = RD 654415

-- instance CyclicCalendar Baha'i where
--   epoch _ = RD 673222

type ModifiedJulianDayNumber :: Type
newtype ModifiedJulianDayNumber = MJD Integer
  deriving stock (Eq, Ord)

instance CyclicCalendar ModifiedJulianDayNumber where
  epoch _ = RD 678576
  fromFixed (RD date) =
    MJD $ date - offset (epoch (Proxy :: Proxy ModifiedJulianDayNumber))
  fromMoment (Moment t) = fromFixed . RD $ floor t

instance Calendar ModifiedJulianDayNumber where
  fixedFrom (MJD mjd) =
    RD $ mjd + offset (epoch (Proxy :: Proxy ModifiedJulianDayNumber))

type Unix :: Type
newtype Unix = SecondsSinceUnixEpoch Second
  deriving stock (Eq, Ord)

instance CyclicCalendar Unix where
  epoch _ = RD 719163
  fromMoment (Moment t) =
    SecondsSinceUnixEpoch $
      24 * 60 * 60 * (t - fromIntegral (offset $ epoch (Proxy :: Proxy Unix)))

instance Calendar Unix where
  momentFrom (SecondsSinceUnixEpoch s) =
    momentFrom (epoch (Proxy :: Proxy Unix)) + Moment (s % 24 % 60 % 60)

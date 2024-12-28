{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_GHC -fplugin-opt=NoRecursion:ignore-decls:newYear,unluckyFridaysInRange,yearFromFixed #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-commercial
--
-- This is the dominant modern calendar. But care must be taken in applying it
-- to historical dates, as different locales adopted the calendar at different
-- times (or switched time zones). So the date in question may actually be a
-- Julian or other date, and so conversion back to Gregorian must be made to
-- identify the actual date.
--
-- __TODO__: Create a separate library that tracks which calendars were used
--           where when, in order to do correct interpretations of historical
--           dates.
module Data.Calendar.Gregorian
  ( Year,
    Date (..),
    Month (..),
    advent,
    christmas,
    dateDifference,
    daylightSavingEnd,
    daylightSavingStart,
    dayNumber,
    daysRemaining,
    electionDay,
    epiphany,
    firstKday,
    independenceDay,
    laborDay,
    lastDayOfMonth,
    lastKday,
    memorialDay,
    newYear,
    nthKday,
    unluckyFridays,
    unluckyFridaysInRange,
    yearEnd,
    yearFromFixed,
    yearRange,
    yearToInteger,
  )
where

import "base" Control.Category ((.))
import "base" Data.Bool (Bool (True), not, (&&), (||))
import "base" Data.Eq (Eq, (==))
import "base" Data.Function (($))
import "base" Data.Kind (Type)
import "base" Data.Ord (Ord, Ordering (EQ, GT, LT), compare, (<), (<=))
import "base" Data.Proxy (Proxy (Proxy))
import "base" Data.Ratio ((%))
import "base" Data.Tuple (fst, snd, uncurry)
import "fin" Data.Fin (Fin)
import "fin" Data.Type.Nat qualified as Nat
import "numeric-tangle" Numeric.Widen (widen)
import "numeric-tangle-fin" Numeric.Widen.Instances.Fin ()
import "this" Data.Calendar
  ( Calendar,
    CyclicCalendar,
    DayOfWeek (Friday, Monday, Sunday, Tuesday),
    FixedDate (RD),
    Moment (Moment),
    Range,
    epoch,
    fixedFrom,
    fromFixed,
    fromMoment,
    kdayAfter,
    kdayBefore,
    kdayNearest,
    kdayOnOrAfter,
    mod,
    mod1,
    offset,
  )
import "this" Data.Calendar.Types
  ( Integer,
    ModularEnum,
    NonnegativeInteger,
    PositiveInteger,
    bogus,
    modularToEnum,
  )
import "base" Prelude
  ( Bounded,
    Enum,
    Num,
    div,
    divMod,
    elem,
    floor,
    fromEnum,
    fromIntegral,
    toEnum,
    (*),
    (+),
    (-),
  )

type Month :: Type
data Month
  = January
  | February
  | March
  | April
  | May
  | June
  | July
  | August
  | September
  | October
  | November
  | December
  deriving stock (Bounded, Eq, Ord)

instance Enum Month where
  fromEnum = \case
    January -> 1
    February -> 2
    March -> 3
    April -> 4
    May -> 5
    June -> 6
    July -> 7
    August -> 8
    September -> 9
    October -> 10
    November -> 11
    December -> 12
  toEnum = modularToEnum

instance ModularEnum Month

type Day :: Type
type Day = Fin (Nat.FromGHC 32)

type Date :: Type
data Date = Date
  {year :: Year, month :: Month, day :: Day}
  deriving stock (Eq, Ord)

instance CyclicCalendar Date where
  epoch _ = RD 1
  fromFixed date = Date {year, month, day}
    where
      year = yearFromFixed date
      priorDays = date - newYear year
      correction =
        if
          | date < fixedFrom (Date year March 1) -> 0
          | isLeapYear year -> 1
          | True -> 2
      month = toEnum $ floor ((12 * (offset priorDays + correction) + 373) % 367)
      day = fromIntegral . offset $ date - fixedFrom (Date year month 1) + 1
  fromMoment (Moment t) = fromFixed . RD $ floor t

instance Calendar Date where
  fixedFrom Date {year, month, day} =
    RD $
      offset (epoch (Proxy :: Proxy Date))
        - 1
        + 365 * (yearToInteger year - 1)
        + floor ((yearToInteger year - 1) % 4)
        - floor ((yearToInteger year - 1) % 100)
        + floor ((yearToInteger year - 1) % 400)
        + floor ((367 * fromEnum month - 362) % 12)
        + ( if
              | month <= February -> 0
              | isLeapYear year -> -1
              | True -> -2
          )
        + widen day

type Year :: Type
newtype Year = Year {yearToInteger :: Integer}
  deriving newtype (Eq, Ord, Num)

isLeapYear :: Year -> Bool
isLeapYear (Year gYear) =
  gYear `mod` 4 == 0 && not ((gYear `mod` 400) `elem` [100, 200, 300])

newYear :: Year -> FixedDate
newYear gYear = fixedFrom $ Date gYear January 1

yearEnd :: Year -> FixedDate
yearEnd gYear = fixedFrom $ Date gYear December 31

-- | (2.20)
yearRange :: Year -> Range
yearRange gYear = (newYear gYear, newYear $ gYear + 1)

yearFromFixed :: FixedDate -> Year
yearFromFixed date = Year $ if n_100 == 4 || n_1 == 4 then year else year + 1
  where
    d_0 = offset $ date - epoch (Proxy :: Proxy Date)
    (n_400, d_1) = d_0 `divMod` 146097
    (n_100, d_2) = d_1 `divMod` 36524
    (n_4, d_3) = d_2 `divMod` 1461
    n_1 = d_3 `div` 365
    year = 400 * n_400 + 100 * n_100 + 4 * n_4 + n_1

dateDifference :: Date -> Date -> Integer
dateDifference gDate1 gDate2 =
  offset (fixedFrom gDate2) - offset (fixedFrom gDate1)

dayNumber :: Date -> PositiveInteger
dayNumber gDate =
  fromIntegral $ dateDifference (Date (year gDate - 1) December 31) gDate

daysRemaining :: Date -> NonnegativeInteger
daysRemaining gDate =
  fromIntegral . dateDifference gDate $ Date (year gDate) December 31

-- | (2.27)
lastDayOfMonth :: Year -> Month -> Day
lastDayOfMonth gYear gMonth =
  fromIntegral . dateDifference (Date gYear gMonth 1) $
    Date
      (if gMonth == December then gYear + 1 else gYear)
      (toEnum $ fromEnum gMonth + 1 `mod1` 12)
      1

independenceDay :: Year -> FixedDate
independenceDay gYear = fixedFrom $ Date gYear July 4

nthKday :: Integer -> DayOfWeek -> Date -> FixedDate
nthKday n k gDate = RD $ case compare n 0 of
  GT -> 7 * n + offset (kdayBefore k fixedDate)
  LT -> 7 * n + offset (kdayAfter k fixedDate)
  EQ -> bogus
  where
    fixedDate = fixedFrom gDate

firstKday :: DayOfWeek -> Date -> FixedDate
firstKday = nthKday 1

lastKday :: DayOfWeek -> Date -> FixedDate
lastKday = nthKday (-1)

laborDay :: Year -> FixedDate
laborDay gYear = firstKday Monday $ Date gYear September 1

memorialDay :: Year -> FixedDate
memorialDay gYear = lastKday Monday $ Date gYear May 31

electionDay :: Year -> FixedDate
electionDay gYear = firstKday Tuesday $ Date gYear November 2

-- |
--
--  __TODO__: I think this needs to be conditionalized on the year.
daylightSavingStart :: Year -> FixedDate
daylightSavingStart gYear = nthKday 2 Sunday $ Date gYear March 1

-- |
--
--  __TODO__: I think this needs to be conditionalized on the year.
daylightSavingEnd :: Year -> FixedDate
daylightSavingEnd gYear = firstKday Sunday $ Date gYear November 1

christmas :: Year -> FixedDate
christmas gYear = fixedFrom $ Date gYear December 25

advent :: Year -> FixedDate
advent gYear = kdayNearest Sunday . fixedFrom $ Date gYear November 30

epiphany :: Year -> FixedDate
epiphany gYear = firstKday Sunday $ Date gYear January 2

unluckyFridaysInRange :: FixedDate -> FixedDate -> [FixedDate]
unluckyFridaysInRange a b =
  if fst range <= fri && fri < snd range
    then
      let laterDays = unluckyFridaysInRange (fri + 1) b
       in if day date == 13
            then fri : laterDays
            else laterDays
    else []
  where
    range = (a, b)
    fri = kdayOnOrAfter Friday a
    date = fromFixed fri

unluckyFridays :: Year -> [FixedDate]
unluckyFridays = uncurry unluckyFridaysInRange . yearRange

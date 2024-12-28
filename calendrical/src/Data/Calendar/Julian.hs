{-# LANGUAGE Safe #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-commercial
module Data.Calendar.Julian
  ( Date (Date),
    Month (..),
    RomanDate,
    RomanYear (AUC),
    Year (BCE, CE),
    aucYearFromJulian,
    easternOrthodoxChristmas,
    inGregorian,
    yearFromAUC,
    yearFromInteger,
    yearToInteger,
    yearRomeFounded,
  )
where

import "base" Control.Category ((.))
import "base" Data.Bifunctor (bimap)
import "base" Data.Bool (Bool (False, True), not, (&&), (||))
import "base" Data.Eq (Eq, (/=), (==))
import "base" Data.Foldable (elem)
import "base" Data.Function (($))
import "base" Data.Functor (fmap)
import "base" Data.Kind (Type)
import "base" Data.Ord (Ord, compare, (<), (<=))
import "base" Data.Proxy (Proxy (Proxy))
import "base" Data.Ratio ((%))
import "fin" Data.Fin (Fin)
import "fin" Data.Type.Nat qualified as Nat
import "numeric-tangle" Numeric.Widen (widen)
import "this" Data.Calendar
  ( Calendar,
    CyclicCalendar,
    FixedDate (RD),
    Moment (Moment),
    epoch,
    fixedFrom,
    fromFixed,
    fromMoment,
    listRange,
    mod,
    mod1,
    momentFrom,
    offset,
  )
import "this" Data.Calendar.Gregorian
  ( Month (December, February, January, July, March, May, November, October, September),
  )
import "this" Data.Calendar.Gregorian qualified as Gregorian
import "this" Data.Calendar.Types (Integer, PositiveInteger)
import "base" Prelude
  ( Bounded,
    abs,
    div,
    floor,
    fromEnum,
    fromIntegral, -- __TODO__: Remove, because it‘s only used in lossy cases.
    toEnum,
    (*),
    (+),
    (-),
  )

-- |
--
--  __NB__: This is a bit more structured than the type from the book … but
--          actually has one more invalid value.
type Year :: Type
data Year
  = BCE PositiveInteger
  | CE PositiveInteger
  deriving stock (Eq)

instance Ord Year where
  compare a b = compare (yearToInteger a) $ yearToInteger b

-- | Turns a Julian year into a contiguous integer representation, using its own
--  `epoch`.
--
--   For example, @`BCE ` 1@ maps to @0@.
--
--  __TOzDO__: Define this as an `Iso'`.
yearToInteger :: Year -> Integer
yearToInteger = \case
  BCE bce -> -(widen bce) + 1
  CE ce -> widen ce

yearFromInteger :: Integer -> Year
yearFromInteger y =
  if y <= 0
    then BCE . fromIntegral $ abs y + 1
    else CE $ fromIntegral y

isLeapYear :: Year -> Bool
isLeapYear = \case
  CE ce -> ce `mod` 4 == 0
  BCE bce -> (-bce) `mod` 4 == 3

type Day :: Type
type Day = Fin (Nat.FromGHC 32)

type Date :: Type
data Date = Date {year :: Year, month :: Month, day :: Day}
  deriving stock (Eq, Ord)

instance CyclicCalendar Date where
  epoch _ = RD (-1)
  fromFixed date = Date year month day
    where
      approx = floor $ (4 * offset (date - epoch (Proxy :: Proxy Date)) + 1464) % 1461
      year = if approx <= 0 then BCE $ approx + 1 else CE approx
      priorDays = offset $ date - fixedFrom (Date year January 1)
      correction =
        if
          | date < fixedFrom (Date year March 1) -> 0
          | isLeapYear year -> 1
          | True -> 2
      month = toEnum . floor $ (12 * (priorDays + correction) + 373) % 367
      day = fromIntegral (offset $ date - fixedFrom (Date year month 1)) + 1
  fromMoment (Moment t) = fromFixed . RD $ floor t

instance Calendar Date where
  fixedFrom Date {year, month, day} =
    RD $
      offset (epoch (Proxy :: Proxy Date))
        - 1
        + 365 * (y - 1)
        + (y - 1 `div` 4)
        + floor ((367 * fromEnum month - 362) % 12)
        + ( if
              | month <= February -> 0
              | isLeapYear year -> -1
              | True -> -2
          )
        + widen day
    where
      y = yearToInteger year

type Event :: Type
data Event
  = Kalends
  | Nones
  | Ides
  deriving stock (Bounded, Eq, Ord)

idesOfMonth :: Month -> Day
idesOfMonth month = if month `elem` [March, May, July, October] then 15 else 13

nonesOfMonth :: Month -> Day
nonesOfMonth month = idesOfMonth month - 8

type RomanDate :: Type
data RomanDate = RomanDate
  { yearR :: Year,
    monthR :: Month,
    event :: Event,
    count :: Fin (Nat.FromGHC 20),
    leap :: Bool
  }
  deriving stock (Eq, Ord)

instance CyclicCalendar RomanDate where
  epoch _ = epoch (Proxy :: Proxy Date)
  fromFixed date
    | day == 1 = RomanDate year month Kalends 1 False
    | day <= nonesOfMonth month =
        RomanDate
          year
          month
          Nones
          (fromIntegral $ nonesOfMonth month - day + 1)
          False
    | day <= idesOfMonth month =
        RomanDate
          year
          month
          Ides
          (fromIntegral $ idesOfMonth month - day + 1)
          False
    | month /= February || not (isLeapYear year) =
        RomanDate
          year'
          month'
          Kalends
          (fromIntegral . offset $ kalends1 - date + 1)
          False
    | day < 25 = RomanDate year March Kalends (fromIntegral $ 30 - day) False
    | True = RomanDate year March Kalends (fromIntegral $ 31 - day) $ day == 25
    where
      Date {year, month, day} = fromFixed date
      month' = toEnum $ fromEnum month + 1 `mod1` 12
      year' =
        if month /= January
          then year
          else case year of
            BCE 1 -> CE 1
            BCE y -> BCE $ y - 1
            CE y -> CE $ y + 1
      kalends1 = fixedFrom $ RomanDate year' month' Kalends 1 False
  fromMoment (Moment t) = fromFixed . RD $ floor t

instance Calendar RomanDate where
  fixedFrom RomanDate {yearR, monthR, event, count, leap} =
    ( case event of
        Kalends -> fixedFrom $ Date yearR monthR 1
        Nones -> fixedFrom . Date yearR monthR $ nonesOfMonth monthR
        Ides -> fixedFrom . Date yearR monthR $ idesOfMonth monthR
    )
      - RD (widen count)
      + ( if isLeapYear yearR
            && monthR == March
            && event == Kalends
            && 6 <= count
            && count <= 16
            then 0
            else 1
        )
      + if leap then 1 else 0

yearRomeFounded :: Year
yearRomeFounded = BCE 753

type RomanYear :: Type
newtype RomanYear = AUC Integer

yearFromAUC :: RomanYear -> Year
yearFromAUC (AUC year) =
  yearFromInteger $
    let romeFounded = yearToInteger yearRomeFounded
     in if 1 <= year && year <= -romeFounded
          then year + romeFounded - 1
          else year + romeFounded

aucYearFromJulian :: Year -> RomanYear
aucYearFromJulian year =
  AUC $
    (yearToInteger year - yearToInteger yearRomeFounded)
      + if yearRomeFounded <= year && year <= BCE 1 then 1 else 0

-- |
--
--  __NB__: The lisp implementation uses `fixedFrom`, but lists the result type
--          as `Moment`, so we prefer to match the types.
inGregorian :: Month -> Day -> Gregorian.Year -> [Moment]
inGregorian jMonth jDay gYear =
  listRange [date0, date1] . bimap momentFrom momentFrom $
    Gregorian.yearRange gYear
  where
    jan1 = Gregorian.newYear gYear
    y = year $ fromFixed jan1
    y' = if y == BCE 1 then CE 1 else yearFromInteger $ yearToInteger y + 1
    date0 = momentFrom $ Date y jMonth jDay
    date1 = momentFrom $ Date y' jMonth jDay

easternOrthodoxChristmas :: Gregorian.Year -> [FixedDate]
easternOrthodoxChristmas = fmap fromMoment . inGregorian December 25

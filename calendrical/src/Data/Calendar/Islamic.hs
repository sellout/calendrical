{-# LANGUAGE Safe #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-commercial
module Data.Calendar.Islamic
  ( Date (Date),
    Day,
    Month (..),
    Year,
    day,
    inGregorian,
    isLeapYear,
    mawlid,
    month,
    year,
  )
where

import "base" Control.Category ((.))
import "base" Data.Bifunctor (bimap)
import "base" Data.Bool (Bool)
import "base" Data.Eq (Eq)
import "base" Data.Function (($))
import "base" Data.Functor (fmap)
import "base" Data.Kind (Type)
import "base" Data.Ord (Ord, (<))
import "base" Data.Proxy (Proxy (Proxy))
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
    momentFrom,
    offset,
  )
import "this" Data.Calendar.Gregorian qualified as Gregorian
import "this" Data.Calendar.Julian qualified as Julian
import "this" Data.Calendar.Types (Integer)
import "base" Prelude
  ( Bounded,
    Enum,
    div,
    floor,
    fromEnum,
    fromIntegral,
    mod,
    toEnum,
    (*),
    (+),
    (-),
  )

type Year :: Type
type Year = Integer

isLeapYear :: Year -> Bool
isLeapYear iYear = (14 + 11 * iYear) `mod` 30 < 11

type Month :: Type
data Month
  = Muharram
  | Safar
  | Rabi'I
  | Rabi'II
  | JumadaI
  | JumadaII
  | Rajab
  | Sha'ban
  | Ramadan
  | Shawwal
  | DhuAlQa'da
  | DhuAlHijja
  deriving stock (Bounded, Enum, Eq, Ord)

type Day :: Type
type Day = Fin (Nat.FromGHC 31)

type Date :: Type
data Date = Date {year :: Year, month :: Month, day :: Day}
  deriving stock (Eq, Ord)

instance CyclicCalendar Date where
  epoch _ = fixedFrom $ Julian.Date (Julian.CE 622) Julian.July 16
  fromFixed date =
    let year = (30 * offset (date - epoch (Proxy :: Proxy Date)) + 10646) `div` 10631
        priorDays = offset $ date - fixedFrom (Date year Muharram 1)
        month = toEnum . fromIntegral $ (11 * priorDays + 330) `div` 325
     in Date
          { year,
            month,
            day =
              fromIntegral . offset $ date - fixedFrom (Date year month 1) + 1
          }
  fromMoment (Moment t) = fromFixed . RD $ floor t

instance Calendar Date where
  fixedFrom Date {year, month, day} =
    RD $
      offset (epoch (Proxy :: Proxy Date))
        - 1
        + (year - 1) * 354
        + (3 + 11 * year) `div` 30
        + 29 * widen (fromEnum month - 1)
        + widen (fromEnum month) `div` 2
        + widen day

inGregorian :: Month -> Day -> Gregorian.Year -> [FixedDate]
inGregorian iMonth iDay gYear =
  fmap fromMoment
    . listRange [date0, date1, date2]
    . bimap momentFrom momentFrom
    $ Gregorian.yearRange gYear
  where
    jan1 = Gregorian.newYear gYear
    y = year $ fromFixed jan1
    date0 = momentFrom $ Date y iMonth iDay
    date1 = momentFrom $ Date (y + 1) iMonth iDay
    date2 = momentFrom $ Date (y + 2) iMonth iDay

mawlid :: Gregorian.Year -> [FixedDate]
mawlid = inGregorian Rabi'I 12

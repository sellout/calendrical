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
import "base" Text.Read (Read)
import "base" Text.Show (Show)
import "fin" Data.Fin (Fin)
import "fin" Data.Type.Nat qualified as Nat
import "numeric-tangle" Numeric.Widen (widen)
import "this" Data.Calendar
  ( Calendar,
    FixedDate (RD),
    LinearCalendar,
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
import "this" Data.Calendar.Types (Integer, ModularEnum, modularToEnum)
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
  deriving stock (Bounded, Eq, Ord, Read, Show)

instance Enum Month where
  fromEnum = \case
    Muharram -> 1
    Safar -> 2
    Rabi'I -> 3
    Rabi'II -> 4
    JumadaI -> 5
    JumadaII -> 6
    Rajab -> 7
    Sha'ban -> 8
    Ramadan -> 9
    Shawwal -> 10
    DhuAlQa'da -> 11
    DhuAlHijja -> 12
  toEnum i = case modularToEnum (Proxy :: Proxy Month) i of
    1 -> Muharram
    2 -> Safar
    3 -> Rabi'I
    4 -> Rabi'II
    5 -> JumadaI
    6 -> JumadaII
    7 -> Rajab
    8 -> Sha'ban
    9 -> Ramadan
    10 -> Shawwal
    11 -> DhuAlQa'da
    _ -> DhuAlHijja

instance ModularEnum Month

type Day :: Type
type Day = Fin (Nat.FromGHC 31)

type Date :: Type
data Date = Date {year :: Year, month :: Month, day :: Day}
  deriving stock (Eq, Ord, Show)

instance Calendar Date where
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

instance LinearCalendar Date where
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

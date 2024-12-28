{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-commercial
module Data.Calendar.Hindu.Old.Solar
  ( Date,
    Day,
    Month,
    Year,
    aryaMonth,
    aryaYear,
    day,
    month,
    year,
  )
where

import "base" Control.Category ((.))
import "base" Data.Eq (Eq)
import "base" Data.Function (($))
import "base" Data.Kind (Type)
import "base" Data.Ord (Ord)
import "base" Data.Proxy (Proxy (Proxy))
import "base" Numeric.Natural (Natural)
import "base" Text.Read (Read)
import "base" Text.Show (Show)
import "fin" Data.Fin (Fin)
import "fin" Data.Type.Nat qualified as Nat
import "numeric-tangle" Numeric.Ration (rationalize, (%))
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
    offset,
  )
import "this" Data.Calendar.Hindu qualified as Hindu
import "this" Data.Calendar.Hindu.Old qualified as Old
import "this" Data.Calendar.Types
  ( Integer,
    ModularEnum,
    Rational,
    hr,
    mod,
    modularToEnum,
    quotient,
  )
import "base" Prelude
  ( Bounded,
    Enum,
    ceiling,
    floor,
    fromEnum,
    fromIntegral,
    toEnum,
    (*),
    (+),
    (-),
  )

-- | Length of Old Hindu solar year.
aryaYear :: Rational
aryaYear = widen $ (1577917500 :: Natural) % 4320000

-- |
--
--   Table 10.1
type Samvatsara :: Type
data Samvatsara
  = Prabhava
  | Vibhava
  | Sukla
  | Pramoda
  | Prajapati
  | Angiras
  | Srimukha
  | Bhava
  | Yuvan
  | Dhatr
  | Isvara
  deriving stock (Bounded, Eq, Ord, Read, Show)

instance Enum Samvatsara where
  fromEnum = \case
    Prabhava -> 1
    Vibhava -> 2
    Sukla -> 3
    Pramoda -> 4
    Prajapati -> 5
    Angiras -> 6
    Srimukha -> 7
    Bhava -> 8
    Yuvan -> 9
    Dhatr -> 10
    Isvara -> 11
  toEnum = modularToEnum

instance ModularEnum Samvatsara

-- | Length of Old Hindu solar month.
aryaMonth :: Rational
aryaMonth = aryaYear % 12

type Year :: Type
type Year = Integer

type Month :: Type
data Month
  = Madhu
  | Madhava
  | Sukra
  | Suchi
  | Nabhas
  | Nabhasya
  | Issa
  | Urja
  | Sahas
  | Sahasya
  | Tapas
  | Tapasya
  deriving stock (Bounded, Eq, Ord, Read, Show)

instance Enum Month where
  fromEnum = \case
    Madhu -> 1
    Madhava -> 2
    Sukra -> 3
    Suchi -> 4
    Nabhas -> 5
    Nabhasya -> 6
    Issa -> 7
    Urja -> 8
    Sahas -> 9
    Sahasya -> 10
    Tapas -> 11
    Tapasya -> 12
  toEnum = modularToEnum

instance ModularEnum Month

type Day :: Type
type Day = Fin (Nat.FromGHC 32)

type Date :: Type
data Date = Date {year :: Year, month :: Month, day :: Day}
  deriving stock (Eq, Ord, Show)

instance CyclicCalendar Date where
  epoch _ = Hindu.epoch
  fromFixed date = Date {year, month, day}
    where
      sun = rationalize (Old.dayCount date) + hr 6
      year = quotient sun aryaYear
      month = toEnum . fromIntegral $ quotient sun aryaMonth `mod` 12 + 1
      day = floor (sun `mod` aryaMonth) + 1
  fromMoment (Moment t) = fromFixed . RD $ floor t

instance Calendar Date where
  fixedFrom Date {year, month, day} =
    RD . ceiling $
      rationalize (offset $ epoch (Proxy :: Proxy Date)) -- Since start of era.
        + rationalize year * aryaYear -- Days in elapsed years
        + widen (rationalize $ fromEnum month - 1) * aryaMonth -- ...in months.
        + rationalize (widen @_ @Integer day)
        + hr -30 -- Midnight of day.

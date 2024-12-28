{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-commercial
module Data.Calendar.Hindu.Old.Lunar
  ( Date,
    Day,
    Leap,
    Month,
    Year,
    day,
    isLeapYear,
    leap,
    month,
    year,
  )
where

import "base" Control.Category ((.))
import "base" Data.Bool (not, (&&))
import "base" Data.Eq (Eq)
import "base" Data.Function (($))
import "base" Data.Kind (Type)
import "base" Data.Ord (Ord, (<=), (>), (>=))
import "base" Data.Proxy (Proxy (Proxy))
import "base" Numeric.Natural (Natural)
import "base" Text.Read (Read)
import "base" Text.Show (Show)
import "fin" Data.Fin (Fin)
import "fin" Data.Type.Nat qualified as Nat
import "numeric-tangle" Numeric.Chop (ceiling, floor)
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
import "this" Data.Calendar.Hindu.Old.Solar qualified as Solar
import "this" Data.Calendar.Types
  ( Boolean,
    Integer,
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
    fromEnum,
    fromIntegral,
    pred,
    toEnum,
    (*),
    (+),
    (-),
  )

type Month :: Type
data Month
  = Caitra
  | Vaisakha
  | Jyestha
  | Asadha
  | Sravana
  | Bhadrapada
  | Asvina
  | Kartika
  | Margasirsa
  | Pausa
  | Magha
  | Phalguna
  deriving stock (Bounded, Eq, Ord, Read, Show)

instance Enum Month where
  fromEnum = \case
    Caitra -> 1
    Vaisakha -> 2
    Jyestha -> 3
    Asadha -> 4
    Sravana -> 5
    Bhadrapada -> 6
    Asvina -> 7
    Kartika -> 8
    Margasirsa -> 9
    Pausa -> 10
    Magha -> 11
    Phalguna -> 12
  toEnum = modularToEnum

instance ModularEnum Month

-- | Length of Old Hindu lunar month.
--
--  (10.9)
aryaMonth :: Rational
aryaMonth = (1577917500 :: Integer) % 53433336

-- | Length of Old Hindu lunar day.
--
--  (10.10)
aryaDay :: Rational
aryaDay = aryaMonth % 30

type Year :: Type
type Year = Integer

type Leap :: Type
type Leap = Boolean

type Day :: Type
type Day = Fin (Nat.FromGHC 31)

type Date :: Type
data Date = Date {year :: Year, month :: Month, leap :: Leap, day :: Day}
  deriving stock (Eq, Ord, Show)

-- | True if @l-year@ is a leap year on the old Hindu calendar.
--
--  (10.11)
isLeapYear :: Year -> Boolean
isLeapYear lYear =
  (rationalize lYear * Solar.aryaYear - Solar.aryaMonth) `mod` aryaMonth
    >= widen ((23902504679 :: Natural) % 1282400064)

instance CyclicCalendar Date where
  epoch _ = Hindu.epoch
  fromFixed date = Date {year, month, leap, day}
    where
      sun = rationalize (Old.dayCount date) + hr 6
      newMoon = sun - sun `mod` aryaMonth
      leap =
        Solar.aryaMonth - aryaMonth >= newMoon `mod` Solar.aryaMonth
          && newMoon `mod` Solar.aryaMonth > 0
      month = toEnum $ fromIntegral (ceiling $ newMoon % Solar.aryaMonth) `mod` 12 + 1
      day = fromIntegral $ quotient sun aryaDay `mod` 30 + 1
      year = ceiling ((newMoon + Solar.aryaMonth) % Solar.aryaYear) - 1
  fromMoment (Moment t) = fromFixed . RD $ floor t

instance Calendar Date where
  fixedFrom Date {year, month, leap, day} =
    RD . ceiling $
      rationalize (offset $ epoch (Proxy :: Proxy Date))
        + newYear
        + aryaMonth
          * ( widen . rationalize . fromEnum $
                if not leap
                  && fromIntegral
                    ( ceiling $
                        (newYear - mina) % (Solar.aryaMonth - aryaMonth)
                    )
                    <= fromEnum month
                  then month
                  else pred month
            )
        + rationalize (widen @_ @Integer day - 1) * aryaDay
        + hr (-6)
    where
      mina = (12 * rationalize year - 1) * Solar.aryaMonth
      newYear = aryaMonth * rationalize (quotient mina aryaMonth + 1)

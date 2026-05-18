{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeApplications #-}
-- This allows the deep `Fin` depths to compile.
{-# OPTIONS_GHC -freduction-depth=0 #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-commercial
--
-- (§11.2)
module Data.Calendar.Mayan.Tzolkin
  ( Date (Date),
    Name (..),
    Number,
    name,
    number,
    ordinal,
    roundOnOrBefore,
    yearBearerFromFixed,
  )
where

import "base" Control.Applicative (empty, pure)
import "base" Control.Category ((.))
import "base" Control.Monad ((=<<))
import "base" Data.Eq (Eq, (==))
import "base" Data.Function (($))
import "base" Data.Kind (Type)
import "base" Data.List.NonEmpty (NonEmpty ((:|)))
import "base" Data.Maybe (Maybe)
import "base" Data.Ord (Ord)
import "base" Data.Proxy (Proxy (Proxy))
import "base" Text.Read (Read)
import "base" Text.Show (Show)
import "fin" Data.Fin (Fin)
import "fin" Data.Type.Nat qualified as Nat
import "numeric-tangle" Numeric.Chop (floor)
import "numeric-tangle" Numeric.Widen (widen)
import "this" Data.Calendar
  ( Calendar,
    CyclicCalendar,
    FixedDate (RD),
    Moment (Moment),
    epoch,
    fixedsFrom,
    fromFixed,
    fromMoment,
    offset,
    onOrBefore,
  )
import "this" Data.Calendar.Mayan qualified as Mayan
import "this" Data.Calendar.Mayan.Haab qualified as Haab
import "this" Data.Calendar.Types
  ( Integer,
    ModularEnum,
    NonnegativeInteger,
    amod,
    mod,
    mod3,
    modularToEnum,
    toModularEnum,
  )
import "base" Prelude (Bounded, Enum, fromEnum, fromIntegral, toEnum, (*), (+), (-))

type Number :: Type
type Number = Fin (Nat.FromGHC 14)

type Name :: Type
data Name
  = -- | “Alligator”
    Imix
  | -- | “Wind”
    Ik
  | -- | “Night”
    Akbal
  | -- | “Iguana”
    Kan
  | -- | “Serpent”
    Chicchan
  | -- | “Death”
    Cimi
  | -- | “Deer”
    Manik
  | -- | “Rabbit”
    Lamat
  | -- | “Rain”
    Muluc
  | -- | “Foot”
    Oc
  | -- | “Monkey”
    Chuen
  | -- | “Tooth”
    Eb
  | -- | “Cane”
    Ben
  | -- | “Jaguar”
    Ix
  | -- | “Eagle”
    Men
  | -- | “Owl”
    Cib
  | -- | “Quake”
    Caban
  | -- | “Flint”
    Etznab
  | -- | “Storm”
    Cauac
  | -- | “Lord”
    Ahau
  deriving stock (Bounded, Eq, Ord, Read, Show)

instance Enum Name where
  fromEnum = \case
    Imix -> 1
    Ik -> 2
    Akbal -> 3
    Kan -> 4
    Chicchan -> 5
    Cimi -> 6
    Manik -> 7
    Lamat -> 8
    Muluc -> 9
    Oc -> 10
    Chuen -> 11
    Eb -> 12
    Ben -> 13
    Ix -> 14
    Men -> 15
    Cib -> 16
    Caban -> 17
    Etznab -> 18
    Cauac -> 19
    Ahau -> 20
  toEnum i = case modularToEnum (Proxy :: Proxy Name) i of
    1 -> Imix
    2 -> Ik
    3 -> Akbal
    4 -> Kan
    5 -> Chicchan
    6 -> Cimi
    7 -> Manik
    8 -> Lamat
    9 -> Muluc
    10 -> Oc
    11 -> Chuen
    12 -> Eb
    13 -> Ben
    14 -> Ix
    15 -> Men
    16 -> Cib
    17 -> Caban
    18 -> Etznab
    19 -> Cauac
    _ -> Ahau

instance ModularEnum Name

type Date :: Type
data Date = Date {number :: Number, name :: Name}
  deriving stock (Eq, Ord, Show)

-- | Number of days into Mayan tzolkin cycle of @tDate@.
--
--  (11.10)
ordinal :: Date -> NonnegativeInteger
ordinal Date {number, name} =
  let signedNumber = widen @NonnegativeInteger @Integer $ widen number
   in widen . Haab.finMod @(Nat.FromGHC 260) $
        signedNumber - 1 + 39 * (signedNumber - widen (fromEnum name))

instance Calendar Date where
  -- (11.8)
  epoch _ = Mayan.epoch - RD (widen . ordinal $ Date 4 Ahau)

  -- (11.9)
  fromFixed date =
    let
     in Date
          { number = fromIntegral (count `amod` 13),
            name = toModularEnum count
          }
    where
      count = offset (date - epoch (Proxy :: Proxy Date)) + 1

  fromMoment (Moment t) = fromFixed . RD $ floor t

  fixedsFrom date =
    let origin = epoch (Proxy :: Proxy Date) + RD (widen $ ordinal date)
     in origin
          :| ( ( \cycle ->
                   let days = 260 * cycle in [origin - days, origin + days]
               )
                 =<< [1 ..]
             )

-- | Mayan tzolkin date of fixed @date@.
--
--  (11.11)
instance CyclicCalendar Date where
  onOrBefore tzolkin (RD date) =
    RD $
      (widen (ordinal tzolkin) + offset (epoch (Proxy :: Proxy Date)))
        `mod3` (date, date - 260)

-- | Year bearer of year containing fixed @date@. Returns `Nothing` for uayeb.
--
--  (11.12)
yearBearerFromFixed :: FixedDate -> Maybe Name
yearBearerFromFixed date =
  if Haab.month (fromFixed date) == Haab.Uayeb
    then empty
    else pure . name . fromFixed $ Haab.Date Haab.Pop 0 `onOrBefore` date

-- | Fixed date of latest date on or before @date@, that is Mayan haab date
--   @haab@ and tzolkin date @tzolkin@. Returns `Nothing` for impossible
--   combinations.
--
--  (11.13)
roundOnOrBefore :: Haab.Date -> Date -> FixedDate -> Maybe FixedDate
roundOnOrBefore haab tzolkin (RD date) =
  if (diff `mod` 5) == 0
    then pure . RD $ (haabCount + 365 * diff) `mod3` (date, date - 18_980)
    else empty -- haab-tzolkin combination is impossible.
  where
    haabCount =
      widen (Haab.ordinal haab) + offset (epoch (Proxy :: Proxy Haab.Date))
    tzolkinCount =
      widen (ordinal tzolkin) + offset (epoch (Proxy :: Proxy Date))
    diff = tzolkinCount - haabCount

{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeApplications #-}
-- This allows the deep `Fin` depths to compile.
{-# OPTIONS_GHC -freduction-depth=0 #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-commercial
--
-- (§11.2)
module Data.Calendar.Mayan.Haab
  ( Month (..),
    Date (Date),
    Day,
    day,
    month,
    ordinal,
    --  FIXME: Don’t eexport
    finMod,
  )
where

import "base" Control.Category ((.))
import "base" Control.Monad ((=<<))
import "base" Data.Eq (Eq)
import "base" Data.Function (($))
import "base" Data.Kind (Type)
import "base" Data.List.NonEmpty (NonEmpty ((:|)))
import "base" Data.Ord (Ord)
import "base" Data.Proxy (Proxy (Proxy))
import "base" Text.Read (Read)
import "base" Text.Show (Show)
import "fin" Data.Fin (Fin)
import "fin" Data.Type.Nat (Nat (S), SNatI)
import "fin" Data.Type.Nat qualified as Nat
import "numeric-tangle" Numeric.Chop (floor)
import "numeric-tangle" Numeric.Ration ((%))
import "numeric-tangle" Numeric.Widen (widen)
import "numeric-tangle-fin" Numeric.Ration.Instances.Fin ()
import "numeric-tangle-fin" Numeric.Widen.Instances.Fin ()
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
import "this" Data.Calendar.Types
  ( ModularEnum,
    NonnegativeInteger,
    mod,
    mod3,
    modularToEnum,
    toModularEnum,
  )
import "base" Prelude
  ( Bounded,
    Enum,
    Integral,
    fromEnum,
    fromIntegral,
    toEnum,
    (*),
    (+),
    (-),
  )
#if MIN_VERSION_GLASGOW_HASKELL(9, 4, 0, 0)
import "base" Data.Type.Equality (type (~))
#endif

-- $setup
-- >>> :seti -XTypeApplications

type Month :: Type
data Month
  = -- | “Mat”
    Pop
  | -- | “Frog”
    Uo
  | -- | “Stag”
    Zip
  | -- | “Bat”
    Zotz
  | -- | “Skull”
    Tzec
  | -- | “End”
    Xul
  | -- | “Green time”
    Yaxkin
  | -- | “Gather”
    Mol
  | -- | “Well”
    Chen
  | -- | “Green”
    Yax
  | -- | “White”
    Zac
  | -- | “Deer”
    Ceh
  | -- | “Cover”
    Mac
  | -- | “Yellow time”
    Kankin
  | -- | “Owl”
    Muan
  | -- | “Drum”
    Pax
  | -- | “Turtle”
    Kayab
  | -- | “Dark god”
    Cumku
  | Uayeb
  deriving stock (Bounded, Eq, Ord, Read, Show)

instance Enum Month where
  fromEnum = \case
    Pop -> 1
    Uo -> 2
    Zip -> 3
    Zotz -> 4
    Tzec -> 5
    Xul -> 6
    Yaxkin -> 7
    Mol -> 8
    Chen -> 9
    Yax -> 10
    Zac -> 11
    Ceh -> 12
    Mac -> 13
    Kankin -> 14
    Muan -> 15
    Pax -> 16
    Kayab -> 17
    Cumku -> 18
    Uayeb -> 19
  toEnum i = case modularToEnum (Proxy :: Proxy Month) i of
    1 -> Pop
    2 -> Uo
    3 -> Zip
    4 -> Zotz
    5 -> Tzec
    6 -> Xul
    7 -> Yaxkin
    8 -> Mol
    9 -> Chen
    10 -> Yax
    11 -> Zac
    12 -> Ceh
    13 -> Mac
    14 -> Kankin
    15 -> Muan
    16 -> Pax
    17 -> Kayab
    18 -> Cumku
    _ -> Uayeb

instance ModularEnum Month

type Day :: Type
type Day = Fin (Nat.FromGHC 20)

type Date :: Type
data Date = Date {month :: Month, day :: Day}
  deriving stock (Eq, Ord, Show)

-- | Number of days into cycle of Mayan haab date @hDate@.
--
--  (11.4)
ordinal :: Date -> NonnegativeInteger
ordinal Date {month, day} = (fromIntegral (fromEnum month) - 1) * 20 + widen day

-- | Convert an `Integral` value to a `Fin`, using the bound as modulus.
--
-- >>> finMod @(Nat.FromGHC 15) 403
-- 13
--
--  __NB__: This is just `fromIntegral`, but specialized to only work on @'S n@
--          (so it’s total) and taking the modulus as the first type parameter.
finMod :: forall n m i. (Integral i, SNatI n, n ~ 'S m) => i -> Fin n
finMod = fromIntegral

instance Calendar Date where
  -- Fixed date of start of haab cycle.
  --
  -- (11.5)
  epoch _ = Mayan.epoch - RD (widen . ordinal $ Date Cumku 8)

  -- Mayan haab date of fixed @date@.
  --
  -- (11.6)
  fromFixed date = Date {month, day}
    where
      count = offset (date - epoch (Proxy :: Proxy Date)) `mod` 365
      day = finMod @(Nat.FromGHC 20) $ count
      month = toModularEnum $ floor (count % 20) + 1

  fromMoment (Moment t) = fromFixed . RD $ floor t

  fixedsFrom date =
    let origin = epoch (Proxy :: Proxy Date) + RD (widen $ ordinal date)
     in origin
          :| ( ( \cycle ->
                   let days = 365 * cycle in [origin - days, origin + days]
               )
                 =<< [1 ..]
             )

-- | Fixed date of latest date on or before fixed @date@ that is Mayan haab date
--   @haab@.
--
--  (11.7)
instance CyclicCalendar Date where
  onOrBefore haab (RD date) =
    RD $
      widen (ordinal haab)
        + offset (epoch (Proxy :: Proxy Date)) `mod3` (date, date - 365)

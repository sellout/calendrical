{-# LANGUAGE Safe #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-commercial
--
-- (§11.3)
module Data.Calendar.Mayan.LongCount
  ( Date (Date),
    date,
  )
where

import "base" Control.Category ((.))
import "base" Data.Bool (Bool (False))
import "base" Data.Eq (Eq)
import "base" Data.Function (($))
import "base" Data.Kind (Type)
import "base" Data.Ord (Ord)
import "base" Text.Show (Show)
import "fin" Data.Fin (Fin)
import "fin" Data.Type.Nat (Nat)
import "fin" Data.Type.Nat qualified as Nat
import "mixed-radix" Numeric.MixedRadix (MixedIntegral (IntRadix, Unbounded), eval, safeInterpret)
import "numeric-tangle-fin" Numeric.Widen.Instances.Fin ()
import "this" Data.Calendar
  ( Calendar,
    FixedDate (RD),
    LinearCalendar,
    Moment (Moment),
    epoch,
    fixedFrom,
    fromFixed,
    fromMoment,
    offset,
  )
import "this" Data.Calendar.Mayan qualified as Mayan
import "base" Prelude (Integer, floor, (+), (-))

type KatunBound :: Nat
type KatunBound = Nat.FromGHC 20

type TunBound :: Nat
type TunBound = Nat.FromGHC 20

type UinalBound :: Nat
type UinalBound = Nat.FromGHC 18

type KinBound :: Nat
type KinBound = Nat.FromGHC 20

type Date :: Type
newtype Date
  = Date (MixedIntegral '[KinBound, UinalBound, TunBound, KatunBound] 'False)
  deriving stock (Eq, Ord, Show)

instance Calendar Date where
  epoch _ = Mayan.epoch

  -- (11.3)
  fromFixed rd = Date . safeInterpret . offset $ rd - Mayan.epoch
  fromMoment (Moment t) = fromFixed . RD $ floor t

instance LinearCalendar Date where
  -- (11.2)
  fixedFrom (Date d) = Mayan.epoch + RD (eval d)

date ::
  Integer ->
  Fin KatunBound ->
  Fin TunBound ->
  Fin UinalBound ->
  Fin KinBound ->
  Date
date baktun katun tun uinal kin =
  Date . IntRadix kin . IntRadix uinal . IntRadix tun . IntRadix katun $
    Unbounded baktun

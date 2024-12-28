{-# LANGUAGE Safe #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-commercial
module Data.Calendar.Ethiopic
  ( module Data.Calendar.Twelve30Plus5.Leap,
    Date,
    Month (..),
  )
where

import "base" Control.Category ((.))
import "base" Data.Eq (Eq)
import "base" Data.Function (($))
import "base" Data.Kind (Type)
import "base" Data.Ord (Ord)
import "this" Data.Calendar
  ( Calendar,
    CyclicCalendar,
    epoch,
    fixedFrom,
    fromFixed,
    fromMoment,
  )
import "this" Data.Calendar.Julian qualified as Julian
import "this" Data.Calendar.Twelve30Plus5.Leap
  ( Day,
    Year,
    date',
    day,
    isLeapYear,
    month,
    year,
  )
import "this" Data.Calendar.Twelve30Plus5.Leap qualified as T30P5
import "base" Prelude (Bounded, Enum)

type Month :: Type
data Month
  = Maskaram
  | Teqemt
  | Hedar
  | Takhsas
  | Ter
  | Yakatit
  | Magabit
  | Miyazya
  | Genbot
  | Sane
  | Hamle
  | Nahase
  | Paguemen
  deriving stock (Bounded, Enum, Eq, Ord)

type Date :: Type
type Date = T30P5.Date Month

ops :: T30P5.Operations Month
ops =
  T30P5.operationsForEpoch . fixedFrom $
    Julian.Date (Julian.CE 8) Julian.August 29

instance CyclicCalendar Date where
  epoch _ = T30P5.epoch ops
  fromFixed = T30P5.fromFixed ops
  fromMoment = T30P5.fromMoment ops

instance Calendar Date where
  fixedFrom = T30P5.fixedFrom ops

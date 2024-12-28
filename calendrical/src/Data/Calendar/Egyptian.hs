{-# LANGUAGE Safe #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-commercial
module Data.Calendar.Egyptian
  ( module Data.Calendar.Twelve30Plus5,
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
    JulianDayNumber (JD),
    epoch,
    fixedFrom,
    fromFixed,
    fromMoment,
  )
import "this" Data.Calendar.Twelve30Plus5 (Day, Year, date', day, month, year)
import "this" Data.Calendar.Twelve30Plus5 qualified as T30P5
import "base" Prelude (Bounded, Enum)

type Month :: Type
data Month
  = Thoth
  | Phaophi
  | Athyr
  | Choiak
  | Tybi
  | Mechir
  | Phamenoth
  | Pharmuthi
  | Pachon
  | Payni
  | Epiphi
  | Mesori
  | Unnamed
  deriving stock (Bounded, Enum, Eq, Ord)

type Date :: Type
type Date = T30P5.Date Month

ops :: T30P5.Operations Month
ops = T30P5.operationsForEpoch . fixedFrom $ JD 1448638

instance CyclicCalendar Date where
  epoch _ = T30P5.epoch ops
  fromFixed = T30P5.fromFixed ops
  fromMoment = T30P5.fromMoment ops

instance Calendar Date where
  fixedFrom = T30P5.fixedFrom ops

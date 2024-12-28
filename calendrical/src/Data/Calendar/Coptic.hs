{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-commercial
module Data.Calendar.Coptic
  ( module Data.Calendar.Twelve30Plus5.Leap,
    Date,
    Month (..),
    christmas,
    inGregorian,
  )
where

import "base" Control.Category ((.))
import "base" Data.Bifunctor (bimap)
import "base" Data.Eq (Eq)
import "base" Data.Function (($))
import "base" Data.Functor (fmap)
import "base" Data.Kind (Type)
import "base" Data.Maybe (maybe)
import "base" Data.Ord (Ord)
import "this" Data.Calendar
  ( Calendar,
    CyclicCalendar,
    FixedDate,
    epoch,
    fixedFrom,
    fromFixed,
    fromMoment,
    listRange,
    momentFrom,
  )
import "this" Data.Calendar.Gregorian qualified as Gregorian
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
import "base" Prelude (Bounded, Enum, error, (+))

type Month :: Type
data Month
  = Thoout
  | Paope
  | Athor
  | Koiak
  | Tobe
  | Meshir
  | Paremotep
  | Parmoute
  | Pashons
  | Paone
  | Epep
  | Mesore
  | Epagomene
  deriving stock (Bounded, Enum, Eq, Ord)

type Date :: Type
type Date = T30P5.Date Month

ops :: T30P5.Operations Month
ops =
  T30P5.operationsForEpoch . fixedFrom $
    Julian.Date (Julian.CE 284) Julian.August 29

instance CyclicCalendar Date where
  epoch _ = T30P5.epoch ops
  fromFixed = T30P5.fromFixed ops
  fromMoment = T30P5.fromMoment ops

instance Calendar Date where
  fixedFrom = T30P5.fixedFrom ops

-- |
--
--  __NB__: This is documented as returning `FixedDate`, but the lisp
--          implementation never rounds it. We do here.
inGregorian :: Month -> Day -> Gregorian.Year -> [FixedDate]
inGregorian cMonth cDay gYear =
  fmap fromMoment . listRange [date0, date1] . bimap momentFrom momentFrom $
    Gregorian.yearRange gYear
  where
    jan1 = Gregorian.newYear gYear
    y = year $ fromFixed @Date jan1
    date0 =
      maybe (error "internal error: illegal date") momentFrom $
        date' y cMonth cDay
    date1 =
      maybe (error "internal error: illegal date") momentFrom $
        date' (y + 1) cMonth cDay

christmas :: Gregorian.Year -> [FixedDate]
christmas = inGregorian Koiak 29

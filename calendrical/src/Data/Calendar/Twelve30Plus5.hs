{-# LANGUAGE Safe #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-commercial
--
-- This is a common calendar structure with a year of twelve thirty-day months
-- followed by a short month of 5–6 days (depending on leap years). Many
-- specific calendars are simply synonyms of this for a specific `Enum` of month
-- names, which also determines the `epoch` and leap-year rule.
module Data.Calendar.Twelve30Plus5
  ( Date (Date),
    Day,
    Operations (Operations),
    Year,
    date',
    day,
    epoch,
    fixedFrom,
    fromFixed,
    fromMoment,
    month,
    operationsForEpoch,
    year,
  )
where

import "base" Control.Applicative (pure)
import "base" Control.Category ((.))
import "base" Data.Bool ((&&), (||))
import "base" Data.Eq (Eq, (==))
import "base" Data.Function (($))
import "base" Data.Functor (fmap)
import "base" Data.Kind (Type)
import "base" Data.Maybe (Maybe (Nothing))
import "base" Data.Ord (Ord, (<))
import "fin" Data.Fin (Fin)
import "fin" Data.Type.Nat qualified as Nat
import "numeric-tangle" Numeric.Widen (widen)
import "numeric-tangle-fin" Numeric.Widen.Instances.Fin ()
import "this" Data.Calendar (FixedDate (RD), Moment (Moment), offset)
import "base" Prelude
  ( Bounded,
    Enum,
    Integer,
    divMod,
    floor,
    fromEnum,
    fromIntegral,
    maxBound,
    toEnum,
    (*),
    (+),
    (-),
  )

type Year :: Type
type Year = Integer

type Day :: Type
type Day = Fin (Nat.FromGHC 32)

-- | CALENDAR TYPE
type Date :: Type -> Type
data Date month = Date {year :: Year, month :: month, day :: Day}
  deriving stock (Eq, Ord)

-- | This is a synonym for the data constructor. It’s necessary, because we
--   can’t import the data constructor without the type constructor, but the
--   type constructor conflicts with a synonym in many other modules.
date' :: (Bounded month, Eq month) => Year -> month -> Day -> Maybe (Date month)
date' y m d =
  if d == 0 || m == maxBound && 5 < d then Nothing else pure $ Date y m d

type Operations :: Type -> Type
data Operations month = Operations
  { epoch :: FixedDate,
    fromFixed :: FixedDate -> Date month,
    fromMoment :: Moment -> Date month,
    fixedFrom :: Date month -> FixedDate
  }

operationsForEpoch :: (Enum month) => FixedDate -> Operations month
operationsForEpoch epoch =
  let fromFixed (RD date) =
        let (year, (month, day)) =
              fmap (`divMod` 30) . (`divMod` 365) $ date - offset epoch
         in Date
              { year = year + 1,
                month = toEnum $ fromIntegral month,
                day = fromIntegral day + 1
              }
   in Operations
        { epoch,
          fromFixed,
          fromMoment = \(Moment t) -> fromFixed . RD $ floor t,
          fixedFrom = \Date {year, month, day} ->
            RD $
              offset epoch
                + 365 * (year - 1)
                + 30 * widen (fromEnum month)
                + widen day
                - 1
        }

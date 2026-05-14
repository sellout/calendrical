{-# LANGUAGE Safe #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-commercial
--
-- (§11.1)
module Data.Calendar.Mayan
  ( epoch,
  )
where

import "base" Data.Function (($))
import "this" Data.Calendar (FixedDate, JulianDayNumber (JD), fixedFrom)

epoch :: FixedDate
epoch = fixedFrom $ JD 584283

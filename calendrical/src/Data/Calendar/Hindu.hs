{-# LANGUAGE Safe #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-commercial
module Data.Calendar.Hindu
  ( epoch,
  )
where

import "base" Data.Function (($))
import "this" Data.Calendar (FixedDate, fixedFrom)
import "this" Data.Calendar.Julian qualified as Julian

epoch :: FixedDate
epoch = fixedFrom $ Julian.Date (Julian.BCE 3102) Julian.February 18

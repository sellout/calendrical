{-# LANGUAGE Safe #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-commercial
module Data.Calendar.Hindu.Old
  ( aryaJovianPeriod,
    dayCount,
    jovianYear,
  )
where

import "base" Data.Function (($))
import "base" Numeric.Natural (Natural)
import "fin" Data.Fin (Fin)
import "fin" Data.Type.Nat qualified as Nat
import "numeric-tangle" Numeric.Ration (rationalize, (%))
import "numeric-tangle" Numeric.Widen (widen)
import "this" Data.Calendar (FixedDate, offset)
import "this" Data.Calendar.Hindu qualified as Hindu
import "this" Data.Calendar.Types (Integer, Rational, amod, quotient)
import "base" Prelude (fromIntegral, (+), (-))

-- | Elapsed days (Ahargana) to @date@ since Hindu epoch (KY).
dayCount :: FixedDate -> Integer
dayCount date = offset $ date - Hindu.epoch

-- | Number of days in one revolution of Jupiter around the Sun.
aryaJovianPeriod :: Rational
aryaJovianPeriod = widen $ (1577917500 :: Natural) % 364224

-- | Year of Jupiter cycle at fixed @date@.
jovianYear ::
  FixedDate ->
  -- | 1–60
  Fin (Nat.FromGHC 61)
jovianYear date =
  fromIntegral $
    (27 + quotient (rationalize $ dayCount date) (aryaJovianPeriod % 12)) `amod` 60

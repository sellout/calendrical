{-# LANGUAGE Safe #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-commercial
module Data.Calendar.Julian.Olympiad
  ( Olympiad (Olympiad),
    cycle,
    fromJulianYear,
    start,
    toJulianYear,
    year,
  )
where

import "base" Data.Function (($))
import "base" Data.Kind (Type)
import "base" Numeric.Natural (Natural)
import "fin" Data.Fin (Fin)
import "fin" Data.Type.Nat qualified as Nat
import "numeric-tangle" Numeric.Widen (widen)
import "this" Data.Calendar.Julian qualified as Julian
import "base" Prelude (divMod, fromIntegral, (*), (+), (-))

type Olympiad :: Type
data Olympiad = Olympiad {cycle :: Natural, year :: Fin (Nat.FromGHC 5)}

start :: Julian.Year
start = Julian.BCE 776

toJulianYear :: Olympiad -> Julian.Year
toJulianYear Olympiad {cycle, year} =
  Julian.yearFromInteger years
  where
    years = Julian.yearToInteger start + 4 * (widen cycle - 1) + widen year - 1

fromJulianYear :: Julian.Year -> Olympiad
fromJulianYear jYear =
  let (cycle, year) = years `divMod` 4
   in Olympiad (fromIntegral $ cycle + 1) (fromIntegral $ year + 1)
  where
    years = Julian.yearToInteger jYear - Julian.yearToInteger start

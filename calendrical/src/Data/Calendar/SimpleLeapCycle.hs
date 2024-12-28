{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-commercial
module Data.Calendar.SimpleLeapCycle
  ( SimpleLeapCycleOperations (..),
    simpleLeapCycleOperations,
  )
where

import "base" Control.Category ((.))
import "base" Data.Bool (Bool)
import "base" Data.Function (($))
import "base" Data.Kind (Type)
import "base" Data.Ord ((<), (<=))
import "base" Data.Ratio (Ratio, Rational)
import "base" Data.Tuple (snd)
import "base" Numeric.Natural (Natural)
import "numeric-tangle" Numeric.Chop (ceiling, floor, mixedFraction)
import "numeric-tangle" Numeric.Ration (rationalize, (%))
import "numeric-tangle" Numeric.Widen (widen)
import "base" Prelude (Integer, div, mod, (*), (+), (-))

-- | CALENDAR TYPE
--
--  __NB__: @unit@ can refer to either day or month, depending on the calendar’s
--          cycle basis.
type SimpleLeapCycleOperations :: Type
data SimpleLeapCycleOperations = SimpleLeapCycleOperations
  { meanYearLength :: Ratio Natural,
    startOfMeanYear0 :: Ratio Natural,
    -- | (1.83)
    isMeanYearLeap :: Integer -> Bool,
    -- | (1.85)
    leapYearsBeforeYear :: Integer -> Integer,
    -- | (1.86)
    unitsBeforeYear :: Integer -> Integer,
    -- | (1.90)
    meanYearContainingUnit :: Integer -> Integer,
    -- | (1.92)
    firstUnitOfYear :: Integer -> Integer,
    -- | (1.93)
    isCalYearLeap :: Integer -> Bool,
    -- | (1.95)
    calYearContainingUnit :: Integer -> Integer
  }

simpleLeapCycleOperations ::
  Natural -> Natural -> Natural -> Natural -> SimpleLeapCycleOperations
simpleLeapCycleOperations c l delta nominalYearLength =
  let -- L̄
      meanYearLength = (c * nominalYearLength + l) % c
      -- δ
      startOfMeanYear0 = snd . mixedFraction $ delta * l % c
      leapYearsBeforeYear y =
        widen l * y - widen l + widen (delta * l `mod` c) `div` widen c
   in SimpleLeapCycleOperations
        { meanYearLength,
          startOfMeanYear0,
          isMeanYearLeap = \y ->
            ((y + widen delta) * widen l `mod` widen c) < widen l,
          leapYearsBeforeYear,
          unitsBeforeYear = \y ->
            leapYearsBeforeYear y + (widen nominalYearLength * (y - 1)),
          meanYearContainingUnit = \n ->
            ceiling
              ( rationalize
                  ( widen c * n
                      + 1
                      + widen (delta * nominalYearLength)
                      + widen (floor $ rationalize delta % (c % l))
                  )
                  % widen meanYearLength
              )
              - widen delta,
          firstUnitOfYear = \y ->
            floor $
              rationalize (y - 1) * widen meanYearLength
                + widen startOfMeanYear0,
          isCalYearLeap = \y ->
            1 - widen (snd $ mixedFraction meanYearLength)
              <= snd (mixedFraction @Rational $ widen startOfMeanYear0 + rationalize (y - 1) * widen meanYearLength),
          calYearContainingUnit = \n ->
            ceiling $
              rationalize n + 1 - widen (startOfMeanYear0 % meanYearLength)
        }

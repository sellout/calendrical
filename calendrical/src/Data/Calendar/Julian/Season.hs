{-# LANGUAGE Safe #-}

-- {-# OPTIONS_GHC -Wno-name-shadowing #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-commercial
module Data.Calendar.Julian.Season
  ( Season,
    spring,
    summer,
    autumn,
    winter,
    degreesMinutesSeconds,
    mins,
    secs,
    degreesFromRadians,
    radiansFromDegrees,
    sinDegrees,
    cosDegrees,
    tanDegrees,
    arctanDegrees,
    cycleInGregorian,
    inGregorian,
  )
where

import "base" Data.Bifunctor (bimap)
import "base" Data.Function (($))
import "base" Data.Kind (Type)
import "numeric-tangle" Numeric.Widen (widen)
import "this" Data.Calendar (Moment (Moment), mod, momentFrom, positionsInRange)
import "this" Data.Calendar.Gregorian qualified as Gregorian
import "this" Data.Calendar.Julian qualified as Julian
import "this" Data.Calendar.Types
  ( Angle,
    PositiveReal,
    arctanDegrees,
    cosDegrees,
    deg,
    degreesFromRadians,
    degreesMinutesSeconds,
    hr,
    mins,
    radiansFromDegrees,
    secs,
    sinDegrees,
    tanDegrees,
  )
import "base" Prelude (fromRational, (*), (+), (-), (/))

type Season :: Type
type Season = Angle

-- | (3.18)
spring :: Season
spring = deg 0

-- | (3.19)
summer :: Season
summer = deg 90

-- | (3.20)
autumn :: Season
autumn = deg 180

-- | (3.21)
winter :: Season
winter = deg 270

-- | (3.22)
cycleInGregorian ::
  Season -> Gregorian.Year -> PositiveReal -> Moment -> [Moment]
cycleInGregorian season gYear nominalYearLength (Moment start) =
  positionsInRange (fromRational pos) nominalYearLength delta year
  where
    year = bimap momentFrom momentFrom $ Gregorian.yearRange gYear
    pos = season / deg 360 * widen nominalYearLength
    delta = fromRational $ pos - (start `mod` widen nominalYearLength)

-- | (3.23)
--
--  __NB__: The book uses `fixedFrom`, but the types are incorrect. This needs
--         `momentFrom`.
inGregorian :: Season -> Gregorian.Year -> [Moment]
inGregorian season gYear =
  cycleInGregorian season gYear y $
    momentFrom (Julian.Date (Julian.BCE 1) Julian.March 23) + offset
  where
    y = fromRational $ 365 + hr 6
    offset = Moment $ season / deg 360 * y

{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeApplications #-}
-- __FIXME__: Remove these options.
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-commercial
--
-- Tooling to help with different astronomical calendars.
module Data.Calendar.Astronomical
  ( Location (Location),
    aberration,
    acre,
    apparentFromLocal,
    apparentFromUniversal,
    daysInJulianCentury,
    declination,
    direction,
    dynamicalFromUniversal,
    elevation,
    ephemerisCorrection,
    equationOfTime,
    greenwich,
    j2000,
    jerusalem,
    julianCenturies,
    latitude,
    localFromApparent,
    localFromStandard,
    localFromUniversal,
    location,
    longitude,
    meanSiderealYear,
    meanTropicalYear,
    mecca,
    midday,
    midnight,
    momentFrom,
    mt,
    nutation,
    obliquity,
    poly,
    precession,
    rightAscension,
    seasonInGregorian,
    secondsInDay,
    secs,
    siderealFromMoment,
    solarLongitude,
    solarLongitudeAfter,
    standardFromLocal,
    standardFromUniversal,
    universalFromApparent,
    universalFromDynamical,
    universalFromLocal,
    universalFromStandard,
    urbana,
    urbanaWinter,
    zone,
    zoneFromLongitude,
  )
where

import "base" Control.Category (id, (.))
import "base" Data.Bool (Bool (True), (&&), (||))
import "base" Data.Eq ((==))
import "base" Data.Function (($))
import "base" Data.Functor (fmap, (<$>))
import "base" Data.Kind (Type)
import "base" Data.Maybe (Maybe)
import "base" Data.Ord (max, min, (<), (<=))
import "base" Numeric (pi)
import "numeric-tangle" Numeric.Ration (rationalize, (%))
import "numeric-tangle" Numeric.Widen (widen)
import "this" Data.Calendar
  ( FixedDate,
    Moment (Moment),
    fromMoment,
    intervalClosed,
    invertAngular,
    momentFrom,
    momentToReal,
    offset,
  )
import "this" Data.Calendar.Gregorian qualified as Gregorian
import "this" Data.Calendar.Julian.Season qualified as Julian (Season, winter)
import "this" Data.Calendar.Types
  ( Angle,
    Duration,
    Integer,
    NonnegativeInteger,
    Real,
    angle,
    arcsinDegrees,
    arctanDegrees,
    cosDegrees,
    deg,
    hr,
    mod,
    poly,
    secs,
    sigma,
    sign,
    sinDegrees,
    tanDegrees,
  )
import "base" Prelude (abs, (*), (+), (-), (/), (^))
import "base" Prelude qualified as Base (Double)

-- | In meters.
mt :: Real -> Distance
mt = id

-- | In the range [-180..180).
--
--  __NB__: Supposedly a subtype of @angle@, but @angle@ has the range [0..360).
type Circle :: Type
type Circle = Angle

type Distance :: Type
type Distance = Real

-- | In the range [-90...90).
type HalfCircle :: Type
type HalfCircle = Circle

-- | In the range [-0.5..0.5).
type FractionOfDay :: Type
type FractionOfDay = Real

type Location :: Type
data Location = Location
  { latitude :: Angle,
    longitude :: Angle,
    elevation :: Distance,
    zone :: FractionOfDay
  }

-- | This only exists because the type names are different in the book.
--
--  __TODO__: Remove this.
location :: HalfCircle -> Circle -> Distance -> Real -> Location
location = Location

urbana, greenwich, mecca, jerusalem, acre :: Location
urbana = Location (deg 40.1) (deg -88.2) (mt 225) $ hr -6
greenwich = Location (deg 51.4777815) (deg 0) (mt 46.9) $ hr 0
mecca = Location (angle 21 25 24) (angle 39 49 24) (mt 298) $ hr 3
jerusalem = Location (deg 31.78) (deg 35.24) (mt 740) $ hr 2
acre = Location (deg 32.94) (deg 35.09) (mt 22) $ hr 2

direction :: Location -> Location -> Angle
direction location focus =
  let phi = latitude location
      phi' = latitude focus
      psi = longitude location
      psi' = longitude focus
      y = sinDegrees $ psi' - psi
      x =
        cosDegrees phi
          * tanDegrees phi'
          - sinDegrees phi
            * cosDegrees (psi - psi')
   in if
        | x == 0 && y == 0 || phi' == deg 90 -> deg 0
        | phi' == deg -90 -> deg 180
        | True -> arctanDegrees y x

zoneFromLongitude :: Circle -> Duration
zoneFromLongitude phi = phi % deg 360

universalFromLocal :: Moment -> Location -> Moment
universalFromLocal tee_ell Location {longitude} =
  tee_ell - Moment (zoneFromLongitude longitude)

localFromUniversal :: Moment -> Location -> Moment
localFromUniversal tee_romU Location {longitude} =
  tee_romU + Moment (zoneFromLongitude longitude)

standardFromUniversal :: Moment -> Location -> Moment
standardFromUniversal tee_romU Location {zone} = tee_romU + Moment zone

universalFromStandard :: Moment -> Location -> Moment
universalFromStandard tee_romS Location {zone} = tee_romS - Moment zone

standardFromLocal :: Moment -> Location -> Moment
standardFromLocal tee_ell location =
  standardFromUniversal (universalFromLocal tee_ell location) location

localFromStandard :: Moment -> Location -> Moment
localFromStandard tee_romS location =
  localFromUniversal (universalFromStandard tee_romS location) location

-- |
--
--  __NB__: Not in the book, but this makes various calculations more obvious.
daysInJulianCentury :: NonnegativeInteger
daysInJulianCentury = 36525

-- |
--
--  __NB__: Not in the book, but this makes various calculations more obvious.
secondsInDay :: NonnegativeInteger
secondsInDay = 24 * 60 * 60

-- |
--
--  __NB__: Not in the book, but this makes various calculations more obvious.
secondsToDay :: Real -> Real
secondsToDay = (% widen (rationalize secondsInDay))

-- |
--
--  __NB__: The implementation in the book has a lot of extra comparisons, which
--          only serve to increase the likelihood of bugs. This should have the
--          same behavior. However, I don’t think that years farther into the
--          future are likely to actually be “other”, but that is what the book
--          shows.
ephemerisCorrection :: Moment -> FractionOfDay
ephemerisCorrection t =
  if
    | 2151 <= year -> other -- should probably be an error
    | 2051 <= year -> c2051
    | 2006 <= year -> c2006
    | 1987 <= year -> c1987
    | 1900 <= year -> c1900
    | 1800 <= year -> c1800
    | 1700 <= year -> c1700
    | 1600 <= year -> c1600
    | 500 <= year -> c500
    | -500 < year -> c0
    | True -> other
  where
    y = Gregorian.yearFromFixed $ fromMoment t
    year = rationalize $ Gregorian.yearToInteger y
    c =
      Gregorian.dateDifference
        (Gregorian.Date 1900 Gregorian.January 1)
        (Gregorian.Date y Gregorian.July 1)
        % widen daysInJulianCentury
    y1820 = (year - 1820) % 100
    c2051 = secondsToDay $ -20 + 32 * y1820 ^ 2 + 0.5628 * (2150 - year)
    y2000 = year - 2000
    c2006 = secondsToDay $ poly y2000 [62.92, 0.32217, 0.005589]
    c1987 =
      secondsToDay $
        poly
          y2000
          [63.86, 0.3345, -0.060374, 0.0017275, 0.000651814, 0.00002373599]
    c1900 =
      poly
        c
        [ -0.00002,
          0.000297,
          0.025184,
          -0.181133,
          0.553040,
          -0.861938,
          0.677066,
          -0.212591
        ]
    c1800 =
      poly
        c
        [ -0.000009,
          0.003844,
          0.083563,
          0.865736,
          4.867575,
          15.845535,
          31.332267,
          38.291999,
          28.316289,
          11.636204,
          2.043794
        ]
    y1700 = year - 1700
    c1700 =
      secondsToDay $
        poly y1700 [8.118780842, -0.005092142, 0.003336121, -0.0000266484]
    y1600 = year - 1600
    c1600 = secondsToDay $ poly y1600 [120, -0.9808, -0.01532, 0.000140272128]
    y1000 = (year - 1000) % 100
    c500 =
      secondsToDay $
        poly
          y1000
          [ 1574.2,
            -556.01,
            71.23472,
            0.319781,
            -0.8503463,
            -0.005050998,
            0.0083572073
          ]
    y0 = year % 100
    c0 =
      secondsToDay $
        poly
          y0
          [ 10583.6,
            -1014.41,
            33.78311,
            -5.952053,
            -0.1798452,
            0.022174192,
            0.0090316521
          ]
    other = secondsToDay $ poly y1820 [-20, 0, 32]

-- | Dynamical time at Universal moment @t_u@.
--
--  (14.16)
dynamicalFromUniversal :: Moment -> Moment
dynamicalFromUniversal t_u = t_u + Moment (ephemerisCorrection t_u)

-- | Universal moment from Dynamical time @tee@.
--
--  (14.17)
universalFromDynamical :: Moment -> Moment
universalFromDynamical t = t - Moment (ephemerisCorrection t)

type Century :: Type
type Century = Real

-- | Julian centuries since 2000 at moment @t@.
--
--  (14.18)
julianCenturies :: Moment -> Century
julianCenturies t =
  momentToReal (dynamicalFromUniversal t - j2000)
    % widen (rationalize daysInJulianCentury)

-- | Noon at start of Gregorian year 2000.
--
--  (14.19)
j2000 :: Moment
j2000 = Moment (hr 12) + momentFrom (Gregorian.newYear 2000)

-- | Equation of time (as fraction of day) for moment @t@.
--
--   Adapted from "Astronomical Algorithms" by Jean Meeus, Willmann-Bell, 2nd
--   edn., 1998, p. 185.
--
--  (14.20)
equationOfTime :: Moment -> FractionOfDay
equationOfTime t = sign equation * min (abs equation) (hr 12)
  where
    c = julianCenturies t
    lambda = poly c $ deg <$> [280.46645, 36000.76983, 0.0003032]
    anomaly = poly c $ deg <$> [357.52910, 35999.05030, -0.0001559, -0.00000048]
    eccentricity = poly c [0.016708617, -0.000042037, -0.0000001236]
    ε = obliquity t
    y = tanDegrees (ε % 2) ^ 2
    equation =
      ( y * sinDegrees (2 * lambda)
          - 2 * eccentricity * sinDegrees anomaly
          + 4 * eccentricity * y * sinDegrees anomaly * cosDegrees (2 * lambda)
          - 0.5 * y ^ 2 * sinDegrees (4 * lambda)
          - 1.25 * eccentricity ^ 2 * sinDegrees (2 * anomaly)
      )
        % (2 * widen @Base.Double pi)

-- | Sundial time from local time @t_l@ at @location@.
--
--  (14.21)
apparentFromLocal :: Moment -> Location -> Moment
apparentFromLocal t_l location =
  t_l + Moment (equationOfTime $ universalFromLocal t_l location)

-- | Local time from sundial time @t@ at @location@.
--
--  (14.22)
localFromApparent :: Moment -> Location -> Moment
localFromApparent t location =
  t - Moment (equationOfTime $ universalFromLocal t location)

-- | True (apparent) time at universal time @t@ at @location@.
--
--  (14.23)
apparentFromUniversal :: Moment -> Location -> Moment
apparentFromUniversal t_u location =
  apparentFromLocal (localFromUniversal t_u location) location

-- | Universal time from sundial time @t@ at @location@.
--
--  (14.24)
universalFromApparent :: Moment -> Location -> Moment
universalFromApparent t location =
  universalFromLocal (localFromApparent t location) location

-- | Universal time of true (apparent) midnight of fixed @date@ at @location@.
--
--  (14.25)
midnight :: FixedDate -> Location -> Moment
midnight = universalFromApparent . momentFrom

-- | Universal time on fixed @date@ of midday at @location@.
--
--  (14.26)
midday :: FixedDate -> Location -> Moment
midday = universalFromApparent . Moment . (+ hr 12) . rationalize . offset

-- | Mean sidereal time of day from moment @t@ expressed as hour angle. Adapted
--   from "Astronomical Algorithms" by Jean Meeus, Willmann-Bell, Inc., 2nd
--   edn., 1998, p. 88.
--
--  (14.27)
siderealFromMoment :: Moment -> Angle
siderealFromMoment t =
  poly c $
    deg
      <$> [ 280.46061837,
            widen (rationalize daysInJulianCentury) * 360.98564736629,
            0.000387933,
            -(1 % (38710000 :: Integer))
          ]
  where
    c = momentToReal (t - j2000) % widen (rationalize daysInJulianCentury)

-- | Obliquity of ecliptic at moment @t@.
--
--  (14.28)
obliquity :: Moment -> Angle
obliquity t =
  angle 23 26 21.448
    + poly c [0, angle 0 0 -46.8150, angle 0 0 -0.00059, angle 0 0 0.001813]
  where
    c = julianCenturies t

-- | Declination at moment UT @t@ of object at latitude @β@ and longitude  @λ@.
--
--  (14.29)
declination :: Moment -> HalfCircle -> Circle -> Angle
declination t β λ =
  arcsinDegrees $
    sinDegrees β * cosDegrees ε + cosDegrees β * sinDegrees ε * sinDegrees λ
  where
    ε = obliquity t

rightAscension :: Moment -> HalfCircle -> Circle -> Angle
rightAscension t β λ =
  arctanDegrees (sinDegrees λ * cosDegrees ε - tanDegrees β * sinDegrees ε) $
    cosDegrees λ
  where
    ε = obliquity t

-- |
--
--  (14.31)
meanTropicalYear :: Duration
meanTropicalYear = 365.242189

-- |
--
--  (14.32)
meanSiderealYear :: Duration
meanSiderealYear = 365.25636

-- | Longitude of sun at moment @t@. Adapted from “Planetary Programs and Tables
--   from -4000 to +2800” by Pierre Bretagnon and Jean-Louis Simon,
--   Willmann-Bell, 1986.
--
--  (14.33)
solarLongitude :: Moment -> Julian.Season
solarLongitude t = (λ + aberration t + nutation t) `mod` 360
  where
    c = julianCenturies t
    λ =
      sigma
        [ (403406, 270.54861, 0.9287892),
          (195207, 340.19128, 35999.1376958),
          (119433, 63.91854, 35999.4089666),
          (112392, 331.26220, 35998.7287385),
          (3891, 317.843, 71998.20261),
          (2819, 86.631, 71998.4403),
          (1721, 240.052, 36000.35726),
          (660, 310.26, 71997.4812),
          (350, 247.23, 32964.4678),
          (334, 260.87, -19.4410),
          (314, 297.82, 445267.1117),
          (268, 343.14, 45036.8840),
          (242, 166.79, 3.1008),
          (234, 81.53, 22518.4434),
          (158, 3.50, -19.9739),
          (132, 132.75, 65928.9345),
          (129, 182.95, 9038.0293),
          (114, 162.03, 3034.7684),
          (99, 29.8, 33718.148),
          (93, 266.4, 3034.448),
          (86, 249.2, -2280.773),
          (78, 157.6, 29929.992),
          (72, 257.8, 31556.493),
          (68, 185.1, 149.588),
          (64, 69.9, 9037.750),
          (46, 8.0, 107997.405),
          (38, 197.1, -4444.176),
          (37, 250.4, 151.771),
          (32, 65.3, 67555.316),
          (29, 162.7, 31556.080),
          (28, 341.5, -4561.540),
          (27, 291.6, 107996.706),
          (27, 98.5, 1221.655),
          (25, 146.7, 62894.167),
          (24, 110.0, 31437.369),
          (21, 5.2, 14578.298),
          (21, 342.6, -31931.757),
          (20, 230.9, 34777.243),
          (18, 256.1, 1221.999),
          (17, 45.3, 62894.511),
          (14, 242.9, -4442.039),
          (13, 115.2, 107997.909),
          (13, 151.8, 119.066),
          (13, 285.3, 16859.071),
          (12, 53.3, -4.578),
          (10, 126.6, 26895.292),
          (10, 205.7, -39.127),
          (10, 85.9, 12297.536),
          (10, 146.1, 90073.778)
        ]
        $ \(x, y, z) -> x * sinDegrees (y + z * c)

-- | Longitudinal nutation at moment @t@.
--
--  (14.34)
nutation :: Moment -> Circle
nutation t = deg -0.004778 * sinDegrees a + deg -0.0003667 * sinDegrees b
  where
    c = julianCenturies t
    a = poly c $ deg <$> [124.90, -1934.134, 0.002063]
    b = poly c $ deg <$> [201.11, 72001.5377, 0.00057]

-- | Aberration at moment @t@.
--
--  (14.35)
aberration :: Moment -> Circle
aberration t =
  deg 0.0000974 * cosDegrees (deg 177.63 + deg 35999.01848 * c) - deg 0.005575
  where
    c = julianCenturies t

solarLongitudeAfter :: Julian.Season -> Moment -> Maybe Moment
solarLongitudeAfter λ t =
  fmap Moment . invertAngular (solarLongitude . Moment) λ $ intervalClosed a b
  where
    rate = meanTropicalYear / deg 360
    τ = t + Moment (rate * (λ - solarLongitude t) `mod` 360)
    a = max t $ τ - 5
    b = τ + 5

-- | Moment UT of @season@ in Gregorian year @gYear@.
--
--  (14.37)
seasonInGregorian :: Julian.Season -> Gregorian.Year -> Maybe Moment
seasonInGregorian season gYear = solarLongitudeAfter season jan1
  where
    jan1 = momentFrom $ Gregorian.newYear gYear

-- |
--
--  (14.38)
urbanaWinter :: Gregorian.Year -> Maybe Moment
urbanaWinter =
  fmap (`standardFromUniversal` urbana) . seasonInGregorian Julian.winter

-- | Precession at moment @t@ using 0,0 as J2000 coordinates. Adapted from
--  "Astronomical Algorithms" by Jean Meeus, Willmann-Bell, 2nd edn., 1998, pp.
--   136-137.
--
--  (14.39)
precession :: Moment -> Angle
precession t = (p + p' - arg) `mod` 360
  where
    c = julianCenturies t
    η = poly c [0, secs 47.0029, secs -0.03302, secs 0.000060] `mod` 360
    p' = poly c [deg 174.876384, secs -869.8089, secs 0.03536] `mod` 360
    p = poly c [0, secs 5029.0966, secs 1.11113, secs 0.000006] `mod` 360
    a = cosDegrees η * sinDegrees p'
    b = cosDegrees p'
    arg = arctanDegrees a b

-- hinduSolarLongitudeAtOrAfter :: Julian.Season -> Moment -> Moment
-- hinduSolarLongitudeAtOrAfter λ t =
--   invertAngular hinduSolarLongitude λ $ intervalClosed a b
--   where
--     τ = t + hinduSiderealYear / (deg 360) * (λ - hinduSolarLongitude t) `mod` 360
--     a = max t $ τ - 5
--     b = τ + 5

-- -- | Fixed moment of Mesha samkranti (Vernal equinox) in Gregorian @gYear@.
-- meshaSamkranti :: Gregorian.Year -> RationalMoment
-- meshaSamkranti = hinduSolarLongitudeAtOrAfter (deg 0) . Gregorian.newYear

-- siderealStart :: Angle
-- siderealStart = precession $ universalFromLocal (meshaSamkranti $ CE 285) hinduLocation

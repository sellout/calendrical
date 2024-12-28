{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -fplugin-opt=NoRecursion:ignore-decls:dayNameOnOrBefore #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-commercial
module Data.Calendar.Akan
  ( Name,
    dayNameOnOrBefore,
    nameDifference,
    prefix,
    stem,
  )
where

import "base" Control.Category ((.))
import "base" Control.Monad ((=<<))
import "base" Data.Function (($))
import "base" Data.Functor.Identity (Identity (Identity))
import "base" Data.Kind (Type)
import "base" Data.List.NonEmpty (NonEmpty ((:|)))
import "base" Data.Proxy (Proxy (Proxy))
import "base" Data.Semigroup ((<>))
import "base" Text.Show (show)
import "this" Data.Calendar
  ( CyclicCalendar,
    FixedDate (RD),
    Moment (Moment),
    epoch,
    fixedsFrom,
    fromFixed,
    fromMoment,
    mod1,
    modI,
    offset,
  )
import "base" Prelude
  ( Bounded,
    Enum,
    Integer,
    error,
    floor,
    fromEnum,
    fromIntegral,
    toEnum,
    (*),
    (+),
    (-),
  )

type Prefix :: Type
data Prefix
  = Nwona
  | Nkyi
  | Kuru
  | Kwa
  | Mono
  | Fo
  deriving stock (Bounded)

instance Enum Prefix where
  fromEnum = \case
    Nwona -> 1
    Nkyi -> 2
    Kuru -> 3
    Kwa -> 4
    Mono -> 5
    Fo -> 6
  toEnum = \case
    1 -> Nwona
    2 -> Nkyi
    3 -> Kuru
    4 -> Kwa
    5 -> Mono
    6 -> Fo
    n -> error $ "invalid Akan day prefix: " <> show n

type Stem :: Type
data Stem
  = Wukuo
  | Yaw
  | Fie
  | Memene
  | Kwasi
  | Dwo
  | Bene
  deriving stock (Bounded)

instance Enum Stem where
  fromEnum = \case
    Wukuo -> 1
    Yaw -> 2
    Fie -> 3
    Memene -> 4
    Kwasi -> 5
    Dwo -> 6
    Bene -> 7
  toEnum = \case
    1 -> Wukuo
    2 -> Yaw
    3 -> Fie
    4 -> Memene
    5 -> Kwasi
    6 -> Dwo
    7 -> Bene
    n -> error $ "invalid Akan day stem: " <> show n

type Name :: Type
data Name = Name {prefix :: Prefix, stem :: Stem}

dayName :: Integer -> Name
dayName n =
  Name (toEnum . fromIntegral $ n `mod1` 6) . toEnum . fromIntegral $
    n `mod1` 7

nameDifference :: Name -> Name -> Integer
nameDifference day1 day2 =
  prefixDifference + 36 * (stemDifference - prefixDifference) `mod1` 42
  where
    prefixDifference = fromIntegral $ fromEnum (prefix day2) - fromEnum (prefix day1)
    stemDifference = fromIntegral $ fromEnum (stem day2) - fromEnum (stem day1)

instance CyclicCalendar Name where
  epoch _ = RD 37
  fromFixed (RD date) = dayName (date - offset (epoch (Proxy :: Proxy Name)))
  fromMoment (Moment t) = fromFixed . RD $ floor t
  fixedsFrom name =
    let origin = dayNameOnOrBefore name . epoch $ Identity name
     in origin
          :| ( ( \cycle ->
                   let days = 42 * cycle in [origin - days, origin + days]
               )
                 =<< [1 ..]
             )

dayNameOnOrBefore :: Name -> FixedDate -> FixedDate
dayNameOnOrBefore name (RD date) =
  RD $ nameDifference (fromFixed $ RD 0) name `modI` (date, date - 42)

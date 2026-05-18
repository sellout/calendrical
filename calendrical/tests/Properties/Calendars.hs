{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: 2026 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-commercial
--
-- Per-calendar wiring for the class-generic property bundles in
-- "Properties.Class".
module Properties.Calendars (allCalendarTests) where

import "base" Data.Eq (Eq)
import "base" Data.Maybe (Maybe (Just, Nothing))
import "base" Data.Proxy (Proxy (Proxy))
import "base" Text.Show (Show)
import "calendrical" Data.Calendar
  ( CyclicCalendar,
    DayOfWeek,
    FixedDate,
    JulianDayNumber,
    LinearCalendar,
    ModifiedJulianDayNumber,
    Unix,
    epoch,
  )
import "calendrical" Data.Calendar.Akan qualified as Akan
import "calendrical" Data.Calendar.Armenian qualified as Armenian
import "calendrical" Data.Calendar.Coptic qualified as Coptic
import "calendrical" Data.Calendar.Egyptian qualified as Egyptian
import "calendrical" Data.Calendar.Ethiopic qualified as Ethiopic
import "calendrical" Data.Calendar.Gregorian qualified as Gregorian
import "calendrical" Data.Calendar.Hindu.Old.Lunar qualified as HinduOldLunar
import "calendrical" Data.Calendar.Hindu.Old.Solar qualified as HinduOldSolar
import "calendrical" Data.Calendar.Icelandic qualified as Icelandic
import "calendrical" Data.Calendar.Islamic qualified as Islamic
import "calendrical" Data.Calendar.Iso qualified as Iso
import "calendrical" Data.Calendar.Julian qualified as Julian
import "calendrical" Data.Calendar.Mayan.Haab qualified as MayanHaab
import "calendrical" Data.Calendar.Mayan.LongCount qualified as MayanLongCount
import "calendrical" Data.Calendar.Mayan.Tzolkin qualified as MayanTzolkin
import "calendrical" Data.Calendar.Types (NonnegativeInteger)
import "calendrical" Data.Calendar.Zoroastrian qualified as Zoroastrian
import "tasty" Test.Tasty (TestTree, testGroup)
import "this" Properties.Class (calendarTests, cyclicTests, linearTests)
import "base" Prelude (Integer, String)

-- | Combine the `Calendar` and `LinearCalendar` properties for one date type.
linearGroup ::
  forall a.
  (LinearCalendar a, Show a) =>
  String ->
  Proxy a ->
  TestTree
linearGroup name p =
  testGroup name [calendarTests p, linearTests p]

-- | Combine the `Calendar` and `CyclicCalendar` properties for one date type.
cyclicGroup ::
  forall a.
  (CyclicCalendar a, Show a, Eq a) =>
  String ->
  Proxy a ->
  Integer ->
  Maybe (a -> NonnegativeInteger, FixedDate) ->
  TestTree
cyclicGroup name p len mOrd =
  testGroup name [calendarTests p, cyclicTests p len mOrd]

allCalendarTests :: TestTree
allCalendarTests =
  testGroup
    "calendar invariants"
    [ linearGroup "FixedDate" (Proxy @FixedDate),
      linearGroup "JulianDayNumber" (Proxy @JulianDayNumber),
      linearGroup "ModifiedJulianDayNumber" (Proxy @ModifiedJulianDayNumber),
      linearGroup "Unix" (Proxy @Unix),
      linearGroup "Gregorian" (Proxy @Gregorian.Date),
      linearGroup "Julian" (Proxy @Julian.Date),
      linearGroup "Julian.RomanDate" (Proxy @Julian.RomanDate),
      linearGroup "Coptic" (Proxy @Coptic.Date),
      linearGroup "Ethiopic" (Proxy @Ethiopic.Date),
      linearGroup "Egyptian" (Proxy @Egyptian.Date),
      linearGroup "Armenian" (Proxy @Armenian.Date),
      linearGroup "Zoroastrian" (Proxy @Zoroastrian.Date),
      linearGroup "Islamic" (Proxy @Islamic.Date),
      linearGroup "Iso" (Proxy @Iso.Date),
      linearGroup "Icelandic" (Proxy @Icelandic.Date),
      linearGroup "Hindu.Old.Solar" (Proxy @HinduOldSolar.Date),
      linearGroup "Hindu.Old.Lunar" (Proxy @HinduOldLunar.Date),
      linearGroup "Mayan.LongCount" (Proxy @MayanLongCount.Date),
      cyclicGroup "DayOfWeek" (Proxy @DayOfWeek) 7 Nothing,
      cyclicGroup "Akan.Name" (Proxy @Akan.Name) 42 Nothing,
      cyclicGroup
        "Mayan.Haab"
        (Proxy @MayanHaab.Date)
        365
        (Just (MayanHaab.ordinal, epoch (Proxy @MayanHaab.Date))),
      cyclicGroup
        "Mayan.Tzolkin"
        (Proxy @MayanTzolkin.Date)
        260
        (Just (MayanTzolkin.ordinal, epoch (Proxy @MayanTzolkin.Date)))
    ]

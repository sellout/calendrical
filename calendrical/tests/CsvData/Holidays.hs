{-# LANGUAGE Trustworthy #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-commercial
--
-- Registry of holiday-function decoders, keyed by the @function@ column in
-- @holiday-list.csv@.
module CsvData.Holidays
  ( holidaySpecs,
    holidayMap,
  )
where

import CsvData.Common
  ( HolidaySpec (HolidaySpec, functionName, holidayFn, holidayLabel),
  )
import "base" Control.Category ((.))
import "base" Data.Eq ((==))
import "base" Data.Functor (fmap)
import "base" Data.Maybe (Maybe (Just, Nothing))
import "base" Data.Semigroup ((<>))
import "calendrical" Data.Calendar (FixedDate, fromFixed)
import "calendrical" Data.Calendar.Coptic qualified as Coptic
import "calendrical" Data.Calendar.Gregorian qualified as Gregorian
import "calendrical" Data.Calendar.Islamic qualified as Islamic
import "calendrical" Data.Calendar.Julian qualified as Julian
import "containers" Data.Map.Strict (Map)
import "containers" Data.Map.Strict qualified as Map
import "text" Data.Text (Text)

holidayMap :: Map Text HolidaySpec
holidayMap =
  Map.fromList (fmap (\spec -> (functionName spec, spec)) holidaySpecs)

holidaySpecs :: [HolidaySpec]
holidaySpecs =
  [ implemented "Advent Sunday" "advent" (just1 . Gregorian.advent),
    implemented "Christmas" "christmas" (just1 . Gregorian.christmas),
    implemented "Epiphany" "epiphany" (just1 . Gregorian.epiphany),
    implemented
      "U.S. Independence Day"
      "independence-day"
      (just1 . Gregorian.independenceDay),
    implemented "U.S. Labor Day" "labor-day" (just1 . Gregorian.laborDay),
    implemented
      "U.S. Memorial Day"
      "memorial-day"
      (just1 . Gregorian.memorialDay),
    implemented
      "U.S. Election Day"
      "election-day"
      (just1 . Gregorian.electionDay),
    implemented
      "U.S. Daylight Savings Start"
      "daylight-saving-start"
      (just1 . Gregorian.daylightSavingStart),
    implemented
      "U.S. Daylight Savings End"
      "daylight-saving-end"
      (just1 . Gregorian.daylightSavingEnd),
    implemented
      "Friday the 13th (first)"
      "unlucky-fridays"
      (firstInGregorianYear . Gregorian.unluckyFridays),
    implemented
      "Christmas (Orthodox)"
      "eastern-orthodox-christmas"
      (firstInGregorianYear . Julian.easternOrthodoxChristmas),
    implemented
      "Christmas (Coptic)"
      "coptic-christmas"
      (firstInGregorianYear . Coptic.christmas),
    implemented "Mawlid" "mawlid" (firstInGregorianYear . Islamic.mawlid)
  ]
    <> pending
  where
    just1 :: FixedDate -> Maybe FixedDate
    just1 = Just
    firstInGregorianYear :: [FixedDate] -> Maybe FixedDate
    firstInGregorianYear xs = case xs of
      [] -> Nothing
      x : _ -> Just x

implemented ::
  Text -> Text -> (Gregorian.Year -> Maybe FixedDate) -> HolidaySpec
implemented lbl fnName fn =
  HolidaySpec
    { holidayLabel = lbl,
      functionName = fnName,
      holidayFn = Just (restrictToYear fn)
    }
  where
    restrictToYear ::
      (Gregorian.Year -> Maybe FixedDate) ->
      Gregorian.Year ->
      Maybe FixedDate
    restrictToYear g year = case g year of
      Nothing -> Nothing
      Just fd ->
        if Gregorian.year (fromFixed fd) == year
          then Just fd
          else Nothing

pending :: [HolidaySpec]
pending =
  fmap
    (\(lbl, name) -> HolidaySpec lbl name Nothing)
    [ ("Baha'i New Year", "bahai-new-year"),
      ("Birkath ha-Hama", "birkath-ha-hama"),
      ("Birth of the Bab", "birth-of-the-bab"),
      ("Birthday of Rama", "rama"),
      ("Chinese New Year", "chinese-new-year"),
      ("Diwali", "diwali"),
      ("Dragon Festival", "dragon-festival"),
      ("Easter", "easter"),
      ("Easter (Astronomical)", "astronomical-easter"),
      ("Easter (Orthodox)", "orthodox-easter"),
      ("Feast of Naw-Ruz", "naw-ruz"),
      ("Feast of Ridvan", "feast-of-ridvan"),
      ("Great Night of Shiva", "shiva"),
      ("Hanukkah (first day)", "hanukkah"),
      ("Hindu Lunar New Year", "hindu-lunar-new-year"),
      ("Icelandic Summer", "icelandic-summer"),
      ("Icelandic Winter", "icelandic-winter"),
      ("Kajeng Keliwon (first)", "kajeng-keliwon"),
      ("Losar", "tibetan-new-year"),
      ("Mesha Samkranti (date)", "mesha-samkranti"),
      ("Nowruz", "nowruz"),
      ("Observ. Hebrew 1 Nisan", "observational-hebrew-first-of-nisan"),
      ("Passover", "passover"),
      ("Passover Eve (Classical)", "classical-passover-eve"),
      ("Pentecost", "pentecost"),
      ("Purim", "purim"),
      ("Qingming", "qing-ming"),
      ("Sacred Wednesday (first)", "sacred-wednesdays"),
      ("Sh'ela", "sh-ela"),
      ("Ta'anit Esther", "ta-anit-esther"),
      ("Tishah be-Av", "tishah-be-av"),
      ("Tumpek (first)", "tumpek"),
      ("Yom ha-Zikkaron", "yom-ha-zikkaron"),
      ("Yom Kippur", "yom-kippur")
    ]

{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-commercial
--
-- Registry of column-group decoders for the CSV-driven test suite. To enable
-- a new calendar once it lands, flip its entry's @decoder@ from 'Nothing' to
-- @Just …@ (and write the decoder).
module CsvData.Calendars
  ( calendarSpecs,
  )
where

import CsvData.Common
  ( CalendarSpec (CalendarSpec, decoder, fields, groupName),
    Decoder,
    fromFixedOnly,
    parseBoolean,
    parseDayOfWeek,
    parseFin,
    parseInteger,
    parseNatural,
    roundTrip,
    stripCell,
  )
import "base" Control.Category ((.))
import "base" Data.Bool (Bool, (&&))
import "base" Data.Either (Either (Left, Right))
import "base" Data.Functor (fmap)
import "base" Data.Int (Int)
import "base" Data.Maybe (Maybe (Just, Nothing))
import "base" Data.Ord ((<=))
import "base" Data.Ratio (Ratio)
import "base" Data.Semigroup ((<>))
import "base" Data.String (String)
import "base" Text.Read (readMaybe)
import "base" Text.Show (show)
import "bytestring" Data.ByteString (ByteString)
import "bytestring" Data.ByteString.Char8 qualified as BSC
import "calendrical" Data.Calendar
  ( DayOfWeek,
    FixedDate,
    JulianDayNumber (JD),
    ModifiedJulianDayNumber (MJD),
    Unix (SecondsSinceUnixEpoch),
    fixedFrom,
    fromFixed,
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
import "calendrical" Data.Calendar.Julian.Olympiad qualified as Olympiad
import "calendrical" Data.Calendar.Twelve30Plus5 qualified as T30P5
import "fin" Data.Fin (Fin)
import "fin" Data.Fin qualified as Fin
import "fin" Data.Type.Nat qualified as Nat
import "base" Prelude
  ( Double,
    Enum,
    Integer,
    fromEnum,
    fromInteger,
    fromIntegral,
    realToFrac,
    toEnum,
    (-),
  )

calendarSpecs :: [CalendarSpec]
calendarSpecs =
  [ CalendarSpec
      { groupName = "Day",
        fields = [],
        decoder = Just dayOfWeekDecoder
      },
    CalendarSpec
      { groupName = "Jd",
        fields = [],
        decoder = Just jdDecoder
      },
    CalendarSpec
      { groupName = "Mjd",
        fields = [],
        decoder = Just mjdDecoder
      },
    CalendarSpec
      { groupName = "Unix",
        fields = [],
        decoder = Just unixDecoder
      },
    CalendarSpec
      { groupName = "Gregorian",
        fields = ["Year", "Month", "Day"],
        decoder = Just gregorianDecoder
      },
    CalendarSpec
      { groupName = "Julian",
        fields = ["Year", "Month", "Day"],
        decoder = Just julianDecoder
      },
    CalendarSpec
      { groupName = "Roman",
        fields = ["Year", "Month", "Event", "Count", "Leap"],
        decoder = Just romanDecoder
      },
    CalendarSpec
      { groupName = "Olympiad",
        fields = ["Cycle", "Year"],
        decoder = Just olympiadDecoder
      },
    CalendarSpec
      { groupName = "Egyptian",
        fields = ["Year", "Month", "Day"],
        decoder = Just egyptianDecoder
      },
    CalendarSpec
      { groupName = "Armenian",
        fields = ["Year", "Month", "Day"],
        decoder = Just armenianDecoder
      },
    CalendarSpec
      { groupName = "Akan Name",
        fields = ["Prefix", "Stem"],
        decoder = Just akanDecoder
      },
    CalendarSpec
      { groupName = "Coptic",
        fields = ["Year", "Month", "Day"],
        decoder = Just copticDecoder
      },
    CalendarSpec
      { groupName = "Ethiopic",
        fields = ["Year", "Month", "Day"],
        decoder = Just ethiopicDecoder
      },
    CalendarSpec
      { groupName = "Iso",
        fields = ["Year", "Week", "Day"],
        decoder = Just isoDecoder
      },
    CalendarSpec
      { groupName = "Icelandic",
        fields = ["Year", "Season", "Week", "Day"],
        decoder = Just icelandicDecoder
      },
    CalendarSpec
      { groupName = "Islamic",
        fields = ["Year", "Month", "Day"],
        decoder = Just islamicDecoder
      },
    CalendarSpec
      { groupName = "Old Hindu Solar",
        fields = ["Year", "Month", "Day"],
        decoder = Just oldHinduSolarDecoder
      },
    CalendarSpec
      { groupName = "Old Hindu Lunar",
        fields = ["Year", "Month", "Leap", "Day"],
        decoder = Just oldHinduLunarDecoder
      },
    CalendarSpec
      { groupName = "Observational Islamic",
        fields = ["Year", "Month", "Day"],
        decoder = Nothing
      },
    CalendarSpec
      { groupName = "Saudi Islamic",
        fields = ["Year", "Month", "Day"],
        decoder = Nothing
      },
    CalendarSpec
      { groupName = "Hebrew",
        fields = ["Year", "Month", "Day"],
        decoder = Nothing
      },
    CalendarSpec
      { groupName = "Observational Hebrew",
        fields = ["Year", "Month", "Day"],
        decoder = Nothing
      },
    CalendarSpec
      { groupName = "Persian",
        fields = ["Year", "Month", "Day"],
        decoder = Nothing
      },
    CalendarSpec
      { groupName = "Arithmetic Persian",
        fields = ["Year", "Month", "Day"],
        decoder = Nothing
      },
    CalendarSpec
      { groupName = "Bahai",
        fields = ["Major", "Cycle", "Year", "Month", "Day"],
        decoder = Nothing
      },
    CalendarSpec
      { groupName = "Astro Bahai",
        fields = ["Major", "Cycle", "Year", "Month", "Day"],
        decoder = Nothing
      },
    CalendarSpec
      { groupName = "French",
        fields = ["Year", "Month", "Day"],
        decoder = Nothing
      },
    CalendarSpec
      { groupName = "Arithmetic French",
        fields = ["Year", "Month", "Day"],
        decoder = Nothing
      },
    CalendarSpec
      { groupName = "Orthodox Easter",
        fields = ["Year", "Month", "Day"],
        decoder = Nothing
      },
    CalendarSpec
      { groupName = "Easter",
        fields = ["Year", "Month", "Day"],
        decoder = Nothing
      },
    CalendarSpec
      { groupName = "Astronomical Easter",
        fields = ["Year", "Month", "Day"],
        decoder = Nothing
      },
    CalendarSpec
      { groupName = "Mayan Long Count",
        fields = ["Baktun", "Katun", "Tun", "Uinal", "Kin"],
        decoder = Nothing
      },
    CalendarSpec
      { groupName = "Mayan Haab",
        fields = ["Month", "Day"],
        decoder = Nothing
      },
    CalendarSpec
      { groupName = "Mayan Tzolkin",
        fields = ["Number", "Name"],
        decoder = Nothing
      },
    CalendarSpec
      { groupName = "Aztec Xihuitl",
        fields = ["Month", "Day"],
        decoder = Nothing
      },
    CalendarSpec
      { groupName = "Aztec Tonalpohualli",
        fields = ["Number", "Name"],
        decoder = Nothing
      },
    CalendarSpec
      { groupName = "Bali Pawukon",
        fields =
          [ "Luang",
            "Dwiwara",
            "Triwara",
            "Caturwara",
            "Pancawara",
            "Sadwara",
            "Saptawara",
            "Asatawara",
            "Sangawara",
            "Dasawara"
          ],
        decoder = Nothing
      },
    CalendarSpec
      { groupName = "Babylonian",
        fields = ["Year", "Month", "Leap", "Day"],
        decoder = Nothing
      },
    CalendarSpec
      { groupName = "Samaritan",
        fields = ["Year", "Month", "Day"],
        decoder = Nothing
      },
    CalendarSpec
      { groupName = "Chinese",
        fields = ["Cycle", "Year", "Month", "Leap", "Day"],
        decoder = Nothing
      },
    CalendarSpec
      { groupName = "Chinese Day Name",
        fields = ["Stem", "Branch"],
        decoder = Nothing
      },
    CalendarSpec
      { groupName = "Next Zhongqi",
        fields = [],
        decoder = Nothing
      },
    CalendarSpec
      { groupName = "Hindu Solar",
        fields = ["Year", "Month", "Day"],
        decoder = Nothing
      },
    CalendarSpec
      { groupName = "Astro Hindu Solar",
        fields = ["Year", "Month", "Day"],
        decoder = Nothing
      },
    CalendarSpec
      { groupName = "Hindu Lunar",
        fields = ["Year", "Month", "Leap", "Day", "Leap"],
        decoder = Nothing
      },
    CalendarSpec
      { groupName = "Astro Hindu Lunar",
        fields = ["Year", "Month", "Leap", "Day", "Leap"],
        decoder = Nothing
      },
    CalendarSpec
      { groupName = "Tibetan",
        fields = ["Year", "Month", "Leap", "Day", "Leap"],
        decoder = Nothing
      },
    CalendarSpec
      { groupName = "Ephem Corr",
        fields = [],
        decoder = Nothing
      },
    CalendarSpec
      { groupName = "Eqn Of Time",
        fields = [],
        decoder = Nothing
      },
    CalendarSpec
      { groupName = "Solar Long",
        fields = [],
        decoder = Nothing
      },
    CalendarSpec
      { groupName = "Solstice",
        fields = [],
        decoder = Nothing
      },
    CalendarSpec
      { groupName = "Dawn",
        fields = ["Moment", "h", "m", "s"],
        decoder = Nothing
      },
    CalendarSpec
      { groupName = "Mid Day",
        fields = ["Moment", "h", "m", "s"],
        decoder = Nothing
      },
    CalendarSpec
      { groupName = "Set",
        fields = ["Moment", "h", "m", "s"],
        decoder = Nothing
      },
    CalendarSpec
      { groupName = "Lunar Long",
        fields = [],
        decoder = Nothing
      },
    CalendarSpec
      { groupName = "Lunar Lat",
        fields = [],
        decoder = Nothing
      },
    CalendarSpec
      { groupName = "Lunar Alt",
        fields = [],
        decoder = Nothing
      },
    CalendarSpec
      { groupName = "New Moon After",
        fields = [],
        decoder = Nothing
      },
    CalendarSpec
      { groupName = "Moonrise",
        fields = ["Moment", "h", "m", "s"],
        decoder = Nothing
      },
    CalendarSpec
      { groupName = "Moonset",
        fields = ["Moment", "h", "m", "s"],
        decoder = Nothing
      }
  ]

------------------------------------------------------------------------------
-- Helpers

parseRational :: ByteString -> Either String (Ratio Integer)
parseRational bs =
  case readMaybe (BSC.unpack (stripCell bs)) :: Maybe Double of
    Just d -> Right (realToFrac d)
    Nothing -> Left ("expected decimal, got " <> show bs)

-- | Parse a 1-based CSV month cell into an 'Enum' whose 'fromEnum' maps the
--   first constructor to @1@ (e.g. 'Gregorian.Month', whose custom 'Enum'
--   gives @fromEnum January == 1@).
parseEnumFromOne :: forall a. (Enum a) => Integer -> ByteString -> Either String a
parseEnumFromOne bound bs = do
  n <- parseInteger bs
  if 1 <= n && n <= bound
    then Right (toEnum (fromIntegral n :: Int))
    else Left ("enum value out of range 1.." <> show bound <> ": " <> show n)

-- | Parse a 1-based CSV month cell into an 'Enum' whose 'fromEnum' maps the
--   first constructor to @0@ (i.e. default-derived 'Enum's like
--   'Egyptian.Month').
parseEnumFromZero :: forall a. (Enum a) => Integer -> ByteString -> Either String a
parseEnumFromZero bound bs = do
  n <- parseInteger bs
  if 1 <= n && n <= bound
    then Right (toEnum (fromIntegral n - 1 :: Int))
    else Left ("enum value out of range 1.." <> show bound <> ": " <> show n)

parseFinOneBased ::
  forall n.
  (Nat.SNatI n) =>
  ByteString ->
  Either String (Fin n)
parseFinOneBased = parseFin -- the CSV uses 1-based week numbers in the
-- range 1..bound, and Fin n stores 0..n-1; the
-- existing parseFin checks 0 <= n < bound, which
-- matches a non-zero Week field.

------------------------------------------------------------------------------
-- Framework-column decoders

dayOfWeekDecoder :: Decoder
dayOfWeekDecoder = fromFixedOnly p (fromFixed @DayOfWeek)
  where
    p :: [ByteString] -> Either String DayOfWeek
    p cs = case cs of
      [c] -> parseDayOfWeek c
      _ -> Left ("Day: expected 1 cell, got " <> show cs)

jdDecoder :: Decoder
jdDecoder = roundTrip parseJd (fromFixed @JulianDayNumber) fixedFrom
  where
    parseJd :: [ByteString] -> Either String JulianDayNumber
    parseJd cs = case cs of
      [c] -> fmap JD (parseRational c)
      _ -> Left ("Jd: expected 1 cell, got " <> show cs)

mjdDecoder :: Decoder
mjdDecoder = roundTrip parseMjd (fromFixed @ModifiedJulianDayNumber) fixedFrom
  where
    parseMjd :: [ByteString] -> Either String ModifiedJulianDayNumber
    parseMjd cs = case cs of
      [c] -> fmap MJD (parseInteger c)
      _ -> Left ("Mjd: expected 1 cell, got " <> show cs)

unixDecoder :: Decoder
unixDecoder = roundTrip parseUnix (fromFixed @Unix) fixedFrom
  where
    parseUnix :: [ByteString] -> Either String Unix
    parseUnix cs = case cs of
      [c] -> fmap (SecondsSinceUnixEpoch . fromInteger) (parseInteger c)
      _ -> Left ("Unix: expected 1 cell, got " <> show cs)

------------------------------------------------------------------------------
-- Calendar decoders

gregorianDecoder :: Decoder
gregorianDecoder = roundTrip parseG fromFixed fixedFrom
  where
    parseG :: [ByteString] -> Either String Gregorian.Date
    parseG cs = case cs of
      [yc, mc, dc] -> do
        y <- parseInteger yc
        m <- parseEnumFromOne @Gregorian.Month 12 mc
        d <- parseFin dc
        Right (Gregorian.Date (fromInteger y) m d)
      _ -> Left ("Gregorian: expected 3 cells, got " <> show cs)

julianDecoder :: Decoder
julianDecoder = roundTrip parseJ fromFixed fixedFrom
  where
    parseJ :: [ByteString] -> Either String Julian.Date
    parseJ cs = case cs of
      [yc, mc, dc] -> do
        y <- parseInteger yc
        m <- parseEnumFromOne @Gregorian.Month 12 mc
        d <- parseFin dc
        Right (Julian.Date (Julian.yearFromInteger y) m d)
      _ -> Left ("Julian: expected 3 cells, got " <> show cs)

romanDecoder :: Decoder
romanDecoder = roundTrip parseR fromFixed fixedFrom
  where
    parseR :: [ByteString] -> Either String Julian.RomanDate
    parseR cs = case cs of
      [yc, mc, ec, cc, lc] -> do
        y <- parseInteger yc
        m <- parseEnumFromOne @Gregorian.Month 12 mc
        e <- parseEvent ec
        cnt <- parseFin cc
        l <- parseBoolean lc
        Right
          Julian.RomanDate
            { Julian.yearR = Julian.yearFromInteger y,
              Julian.monthR = m,
              Julian.event = e,
              Julian.count = cnt,
              Julian.leap = l
            }
      _ -> Left ("Roman: expected 5 cells, got " <> show cs)
    parseEvent :: ByteString -> Either String Julian.Event
    parseEvent bs = do
      n <- parseInteger bs
      case n of
        1 -> Right Julian.Kalends
        2 -> Right Julian.Nones
        3 -> Right Julian.Ides
        _ -> Left ("Roman Event must be 1|2|3, got " <> show n)

olympiadDecoder :: Decoder
olympiadDecoder = fromFixedOnly parseO derive
  where
    parseO :: [ByteString] -> Either String Olympiad.Olympiad
    parseO cs = case cs of
      [cc, yc] -> do
        cyc <- parseNatural cc
        y <- parseOlympiadYear yc
        Right (Olympiad.Olympiad cyc y)
      _ -> Left ("Olympiad: expected 2 cells, got " <> show cs)
    parseOlympiadYear ::
      ByteString -> Either String (Fin (Nat.FromGHC 5))
    parseOlympiadYear bs = do
      n <- parseInteger bs
      if 1 <= n && n <= 4
        then Right (fromInteger n)
        else Left ("Olympiad year must be 1..4, got " <> show n)
    derive :: FixedDate -> Olympiad.Olympiad
    derive = Olympiad.fromJulianYear . Julian.year . fromFixed @Julian.Date

egyptianDecoder :: Decoder
egyptianDecoder = roundTrip parseE fromFixed fixedFrom
  where
    parseE :: [ByteString] -> Either String Egyptian.Date
    parseE cs = case cs of
      [yc, mc, dc] -> do
        y <- parseInteger yc
        m <- parseEnumFromZero @Egyptian.Month 13 mc
        d <- parseFin dc
        Right T30P5.Date {T30P5.year = y, T30P5.month = m, T30P5.day = d}
      _ -> Left ("Egyptian: expected 3 cells, got " <> show cs)

armenianDecoder :: Decoder
armenianDecoder = roundTrip parseA fromFixed fixedFrom
  where
    parseA :: [ByteString] -> Either String Armenian.Date
    parseA cs = case cs of
      [yc, mc, dc] -> do
        y <- parseInteger yc
        m <- parseEnumFromZero @Armenian.Month 13 mc
        d <- parseFin dc
        Right T30P5.Date {T30P5.year = y, T30P5.month = m, T30P5.day = d}
      _ -> Left ("Armenian: expected 3 cells, got " <> show cs)

copticDecoder :: Decoder
copticDecoder = roundTrip parseC fromFixed fixedFrom
  where
    parseC :: [ByteString] -> Either String Coptic.Date
    parseC cs = case cs of
      [yc, mc, dc] -> do
        y <- parseInteger yc
        m <- parseEnumFromZero @Coptic.Month 13 mc
        d <- parseFin dc
        Right T30P5.Date {T30P5.year = y, T30P5.month = m, T30P5.day = d}
      _ -> Left ("Coptic: expected 3 cells, got " <> show cs)

ethiopicDecoder :: Decoder
ethiopicDecoder = roundTrip parseE fromFixed fixedFrom
  where
    parseE :: [ByteString] -> Either String Ethiopic.Date
    parseE cs = case cs of
      [yc, mc, dc] -> do
        y <- parseInteger yc
        m <- parseEnumFromZero @Ethiopic.Month 13 mc
        d <- parseFin dc
        Right T30P5.Date {T30P5.year = y, T30P5.month = m, T30P5.day = d}
      _ -> Left ("Ethiopic: expected 3 cells, got " <> show cs)

isoDecoder :: Decoder
isoDecoder = roundTrip parseI fromFixed fixedFrom
  where
    parseI :: [ByteString] -> Either String Iso.Date
    parseI cs = case cs of
      [yc, wc, dc] -> do
        y <- parseInteger yc
        w <- parseFinOneBased wc
        dow <- parseIsoDay dc
        Right (Iso.Date (fromInteger y) w dow)
      _ -> Left ("Iso: expected 3 cells, got " <> show cs)
    parseIsoDay :: ByteString -> Either String DayOfWeek
    parseIsoDay bs = do
      n <- parseInteger bs
      if 1 <= n && n <= 7
        then Right (toEnum (fromIntegral n :: Int))
        else Left ("ISO day must be 1..7, got " <> show n)

icelandicDecoder :: Decoder
icelandicDecoder = roundTrip parseI fromFixed fixedFrom
  where
    parseI :: [ByteString] -> Either String Icelandic.Date
    parseI cs = case cs of
      [yc, sc, wc, dc] -> do
        y <- parseInteger yc
        s <- parseSeason sc
        w <- parseFinOneBased wc
        dow <- parseIcelandicWeekday dc
        Right
          Icelandic.Date
            { Icelandic.year = y,
              Icelandic.season = s,
              Icelandic.week = w,
              Icelandic.weekday = dow
            }
      _ -> Left ("Icelandic: expected 4 cells, got " <> show cs)
    parseSeason :: ByteString -> Either String Icelandic.Season
    parseSeason bs = do
      n <- parseInteger bs
      case n of
        1 -> Right Icelandic.Summer
        2 -> Right Icelandic.Winter
        _ -> Left ("Icelandic Season must be 1|2, got " <> show n)
    parseIcelandicWeekday :: ByteString -> Either String DayOfWeek
    parseIcelandicWeekday bs = do
      n <- parseInteger bs
      if 0 <= n && n <= 6
        then Right (toEnum (fromIntegral n :: Int))
        else Left ("Icelandic Day must be 0..6, got " <> show n)

islamicDecoder :: Decoder
islamicDecoder = roundTrip parseI fromFixed fixedFrom
  where
    parseI :: [ByteString] -> Either String Islamic.Date
    parseI cs = case cs of
      [yc, mc, dc] -> do
        y <- parseInteger yc
        m <- parseEnumFromZero @Islamic.Month 12 mc
        d <- parseFin dc
        Right
          Islamic.Date
            { Islamic.year = y,
              Islamic.month = m,
              Islamic.day = d
            }
      _ -> Left ("Islamic: expected 3 cells, got " <> show cs)

akanDecoder :: Decoder
akanDecoder = fromFixedOnly parseN (fromFixed @Akan.Name)
  where
    parseN :: [ByteString] -> Either String Akan.Name
    parseN cs = case cs of
      [pc, sc] -> do
        pn <- parseInteger pc
        sn <- parseInteger sc
        if 1 <= pn && pn <= 6 && 1 <= sn && sn <= 7
          then
            Right
              Akan.Name
                { Akan.prefix = toEnum (fromIntegral pn :: Int),
                  Akan.stem = toEnum (fromIntegral sn :: Int)
                }
          else
            Left
              ( "Akan Prefix/Stem out of range: "
                  <> show pn
                  <> "/"
                  <> show sn
              )
      _ -> Left ("Akan Name: expected 2 cells, got " <> show cs)

oldHinduSolarDecoder :: Decoder
oldHinduSolarDecoder = fromFixedOnly parseS derive
  where
    parseS :: [ByteString] -> Either String (Integer, Integer, Integer)
    parseS cs = case cs of
      [yc, mc, dc] -> do
        y <- parseInteger yc
        m <- parseInteger mc
        d <- parseInteger dc
        Right (y, m, d)
      _ -> Left ("Old Hindu Solar: expected 3 cells, got " <> show cs)
    derive :: FixedDate -> (Integer, Integer, Integer)
    derive rd =
      let d = fromFixed @HinduOldSolar.Date rd
       in ( HinduOldSolar.year d,
            fromIntegral (fromEnum (HinduOldSolar.month d)),
            Fin.toInteger (HinduOldSolar.day d)
          )

oldHinduLunarDecoder :: Decoder
oldHinduLunarDecoder = fromFixedOnly parseL derive
  where
    parseL ::
      [ByteString] -> Either String (Integer, Integer, Bool, Integer)
    parseL cs = case cs of
      [yc, mc, lc, dc] -> do
        y <- parseInteger yc
        m <- parseInteger mc
        l <- parseBoolean lc
        d <- parseInteger dc
        Right (y, m, l, d)
      _ -> Left ("Old Hindu Lunar: expected 4 cells, got " <> show cs)
    derive :: FixedDate -> (Integer, Integer, Bool, Integer)
    derive rd =
      let d = fromFixed @HinduOldLunar.Date rd
       in ( HinduOldLunar.year d,
            fromIntegral (fromEnum (HinduOldLunar.month d)),
            HinduOldLunar.leap d,
            Fin.toInteger (HinduOldLunar.day d)
          )

{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-commercial
--
-- Infrastructure for the CSV-driven test suite.
--
-- Each CSV in @tests/data/@ ships test data from the /Calendrical Calculations/
-- reference. The @Rd@ column is the source of truth; every other column-group
-- names a calendar (or framework type) whose conversion from\/to that @Rd@ we
-- want to verify.
module CsvData.Common
  ( -- * Spec types
    Decoder (Decoder, runDecoder),
    CalendarSpec (CalendarSpec, groupName, fields, decoder),
    HolidaySpec (HolidaySpec, holidayLabel, functionName, holidayFn),

    -- * CSV loading
    GroupMap,
    loadCsv,
    parseHeader1,
    parseHeader2,
    parseHolidayYearColumns,

    -- * Cell parsers
    stripCell,
    parseInt,
    parseInteger,
    parseNatural,
    parseBoolean,
    parseFin,
    parseDayOfWeek,

    -- * Decoder builders
    roundTrip,
    fromFixedOnly,

    -- * Test tree builders
    rowTests,
    holidayTests,
  )
where

import "base" Control.Applicative (pure)
import "base" Control.Category ((.))
import "base" Control.Monad (unless)
import "base" Data.Bool (Bool (False, True), (&&))
import "base" Data.Char (Char, isSpace)
import "base" Data.Either (Either (Left, Right), either)
import "base" Data.Eq (Eq, (==))
import "base" Data.Foldable (foldl')
import "base" Data.Function (($))
import "base" Data.Functor (fmap)
import "base" Data.Int (Int)
import "base" Data.Kind (Type)
import "base" Data.List (drop, take, zip)
import "base" Data.Maybe (Maybe (Just, Nothing), mapMaybe)
import "base" Data.Ord ((<), (<=), (>=))
import "base" Data.Proxy (Proxy (Proxy))
import "base" Data.Semigroup ((<>))
import "base" Data.String (String)
import "base" Numeric.Natural (Natural)
import "base" System.IO (FilePath, IO)
import "base" Text.Read (readMaybe)
import "base" Text.Show (Show, show)
import "bytestring" Data.ByteString qualified as BS
import "bytestring" Data.ByteString.Char8 qualified as BSC
import "bytestring" Data.ByteString.Lazy qualified as BL
import "calendrical" Data.Calendar
  ( DayOfWeek
      ( Friday,
        Monday,
        Saturday,
        Sunday,
        Thursday,
        Tuesday,
        Wednesday
      ),
    FixedDate (RD),
    fromFixed,
  )
import "calendrical" Data.Calendar.Gregorian qualified as Gregorian
import "cassava" Data.Csv qualified as Csv
import "containers" Data.Map.Strict (Map)
import "containers" Data.Map.Strict qualified as Map
import "fin" Data.Fin (Fin)
import "fin" Data.Fin qualified as Fin
import "fin" Data.Type.Nat qualified as Nat
import "tasty" Test.Tasty (TestName, TestTree, testGroup)
import "tasty-expected-failure" Test.Tasty.ExpectedFailure (ignoreTestBecause)
import "tasty-hunit" Test.Tasty.HUnit (Assertion, assertFailure, testCase)
import "text" Data.Text (Text)
import "text" Data.Text qualified as T
import "vector" Data.Vector qualified as V
import "base" Prelude (Integer, fromInteger, (+))

-- | A decoder reads the slice of cells belonging to one calendar's
--   column-group, plus the row's @Rd@, and either returns 'Right' (test
--   passes) or 'Left' with a message (test fails).
type Decoder :: Type
newtype Decoder = Decoder
  {runDecoder :: FixedDate -> [BS.ByteString] -> Either String ()}

-- | One column-group's entry in the registry.
type CalendarSpec :: Type
data CalendarSpec = CalendarSpec
  { groupName :: Text,
    fields :: [Text],
    decoder :: Maybe Decoder
  }

-- | One holiday's entry in the registry.
type HolidaySpec :: Type
data HolidaySpec = HolidaySpec
  { holidayLabel :: Text,
    functionName :: Text,
    holidayFn :: Maybe (Gregorian.Year -> Maybe FixedDate)
  }

-- | Maps the row-1 header label of a column-group to its column range
--   (start inclusive, end exclusive) in a CSV row.
type GroupMap :: Type
type GroupMap = Map Text (Int, Int)

-- | Strip surrounding ASCII whitespace from a cell.
stripCell :: BS.ByteString -> BS.ByteString
stripCell = BSC.dropWhile isSp . dropEndWhile isSp
  where
    isSp :: Char -> Bool
    isSp = isSpace
    dropEndWhile :: (Char -> Bool) -> BS.ByteString -> BS.ByteString
    dropEndWhile p = BS.reverse . BSC.dropWhile p . BS.reverse

-- | Load a CSV file as a list of rows, where each row is a list of stripped
--   cells with the trailing empty cell (from the dataset's trailing comma)
--   dropped.
loadCsv :: FilePath -> IO [[BS.ByteString]]
loadCsv fp = do
  bs <- BL.readFile fp
  case Csv.decode Csv.NoHeader bs of
    Left e -> do
      _ <- assertFailure ("CSV parse error in " <> fp <> ": " <> e)
      pure []
    Right rows -> pure (fmap normalize (V.toList rows))
  where
    -- Strip each cell, but keep trailing empty cells: row lengths in the
    -- dataset aren't consistent (some lines have a trailing comma, others
    -- don't), and the @Map Text (Int, Int)@ slicing relies on positional
    -- column indexes that are stable across all rows.
    normalize :: V.Vector BS.ByteString -> [BS.ByteString]
    normalize = fmap stripCell . V.toList

-- | Parse the first row of a two-row header. Each non-empty cell starts a
--   column-group; subsequent empty cells extend the group's range.
parseHeader1 :: [BS.ByteString] -> GroupMap
parseHeader1 cells =
  Map.fromList (close (foldl' step (0, Nothing, []) (zip [0 :: Int ..] cells)))
  where
    step ::
      (Int, Maybe (Text, Int), [(Text, (Int, Int))]) ->
      (Int, BS.ByteString) ->
      (Int, Maybe (Text, Int), [(Text, (Int, Int))])
    step (_, open, acc) (i, cell) =
      if BS.null cell
        then (i + 1, open, acc)
        else case open of
          Nothing -> (i + 1, Just (textOf cell, i), acc)
          Just (prev, prevStart) ->
            (i + 1, Just (textOf cell, i), (prev, (prevStart, i)) : acc)
    close ::
      (Int, Maybe (Text, Int), [(Text, (Int, Int))]) ->
      [(Text, (Int, Int))]
    close (total, open, acc) = case open of
      Nothing -> acc
      Just (name, start) -> (name, (start, total)) : acc
    textOf :: BS.ByteString -> Text
    textOf = T.pack . BSC.unpack

parseHeader2 :: (Int, Int) -> [BS.ByteString] -> [Text]
parseHeader2 (start, end) row =
  fmap (T.pack . BSC.unpack) (drop start (take end row))

-- | Parse the holiday CSV's one-row header into @(year, monthCol, dayCol)@
--   triples.
parseHolidayYearColumns :: [BS.ByteString] -> [(Gregorian.Year, Int, Int)]
parseHolidayYearColumns header = mapMaybe build (zip [0 :: Int ..] header)
  where
    build :: (Int, BS.ByteString) -> Maybe (Gregorian.Year, Int, Int)
    build (i, cell) = case readMaybe (BSC.unpack cell) :: Maybe Integer of
      Just y -> Just (fromInteger y, i, i + 1)
      Nothing -> Nothing

parseInt :: BS.ByteString -> Either String Int
parseInt bs = case readMaybe (BSC.unpack (stripCell bs)) of
  Just n -> Right n
  Nothing -> Left ("expected int, got " <> show bs)

parseInteger :: BS.ByteString -> Either String Integer
parseInteger bs = case readMaybe (BSC.unpack (stripCell bs)) of
  Just n -> Right n
  Nothing -> Left ("expected integer, got " <> show bs)

parseNatural :: BS.ByteString -> Either String Natural
parseNatural bs = do
  n <- parseInteger bs
  if n >= 0
    then Right (fromInteger n)
    else Left ("expected non-negative integer, got " <> show n)

parseBoolean :: BS.ByteString -> Either String Bool
parseBoolean bs = case stripCell bs of
  s | s == BSC.pack "f" -> Right False
  s | s == BSC.pack "t" -> Right True
  _ -> Left ("expected `f` or `t`, got " <> show bs)

parseFin ::
  forall n.
  (Nat.SNatI n) =>
  BS.ByteString ->
  Either String (Fin n)
parseFin bs = do
  n <- parseInteger bs
  let bound :: Integer
      bound = Nat.reflectToNum (Proxy :: Proxy n)
  if 0 <= n && n < bound
    then Right (fromInteger n)
    else
      Left
        ( "Fin out of range: expected 0 <= "
            <> show n
            <> " < "
            <> show bound
        )

parseDayOfWeek :: BS.ByteString -> Either String DayOfWeek
parseDayOfWeek bs = case stripCell bs of
  s | s == BSC.pack "Sunday" -> Right Sunday
  s | s == BSC.pack "Monday" -> Right Monday
  s | s == BSC.pack "Tuesday" -> Right Tuesday
  s | s == BSC.pack "Wednesday" -> Right Wednesday
  s | s == BSC.pack "Thursday" -> Right Thursday
  s | s == BSC.pack "Friday" -> Right Friday
  s | s == BSC.pack "Saturday" -> Right Saturday
  _ -> Left ("expected day-of-week, got " <> show bs)

roundTrip ::
  forall d.
  (Eq d, Show d) =>
  ([BS.ByteString] -> Either String d) ->
  (FixedDate -> d) ->
  (d -> FixedDate) ->
  Decoder
roundTrip parseCells fromRd toRd = Decoder $ \rd cells -> do
  parsed <- parseCells cells
  let computed = fromRd rd
  unless (computed == parsed) . Left $
    "fromFixed mismatch: parsed "
      <> show parsed
      <> ", computed "
      <> show computed
  let back = toRd parsed
  unless (back == rd) . Left $
    "fixedFrom mismatch: parsed "
      <> show parsed
      <> " yields "
      <> show back
      <> ", expected "
      <> show rd

fromFixedOnly ::
  forall d.
  (Eq d, Show d) =>
  ([BS.ByteString] -> Either String d) ->
  (FixedDate -> d) ->
  Decoder
fromFixedOnly parseCells fromRd = Decoder $ \rd cells -> do
  parsed <- parseCells cells
  let computed = fromRd rd
  unless (computed == parsed) . Left $
    "fromFixed mismatch: parsed "
      <> show parsed
      <> ", computed "
      <> show computed

-- | Build the 'TestTree' for one row of a @dates*.csv@ / @\<year\>.csv@ file.
rowTests :: GroupMap -> [CalendarSpec] -> [BS.ByteString] -> TestTree
rowTests groups specs row = case row of
  [] -> testCase "<empty row>" (assertFailure "empty CSV row")
  rdCell : _ -> case parseInteger rdCell of
    Left e -> testCase ("row <" <> BSC.unpack rdCell <> ">") (assertFailure e)
    Right rdInt ->
      testGroup
        ("Rd " <> show rdInt)
        (mapMaybe (columnTest groups row (RD rdInt)) specs)

columnTest ::
  GroupMap ->
  [BS.ByteString] ->
  FixedDate ->
  CalendarSpec ->
  Maybe TestTree
columnTest groups row rd CalendarSpec {groupName, decoder} =
  fmap (build . sliceFor) (Map.lookup groupName groups)
  where
    sliceFor :: (Int, Int) -> [BS.ByteString]
    sliceFor (start, end) = drop start (take end row)
    tname :: TestName
    tname = T.unpack groupName
    build :: [BS.ByteString] -> TestTree
    build cells = case decoder of
      Nothing ->
        ignoreTestBecause
          ("pending: " <> tname <> " not yet implemented")
          (testCase tname (pure ()))
      Just (Decoder run) ->
        testCase tname (eitherToAssertion (run rd cells))

eitherToAssertion :: Either String () -> Assertion
eitherToAssertion = either assertFailure pure

-- | Build the 'TestTree' for one row of @holiday-list.csv@.
holidayTests ::
  [(Gregorian.Year, Int, Int)] ->
  Map Text HolidaySpec ->
  [BS.ByteString] ->
  TestTree
holidayTests yearCols specMap row = case row of
  labelCell : fnCell : _ ->
    let fnName :: Text
        fnName = T.pack (BSC.unpack fnCell)
        treeName :: TestName
        treeName =
          BSC.unpack labelCell <> " (" <> T.unpack fnName <> ")"
     in case Map.lookup fnName specMap of
          Nothing ->
            ignoreTestBecause
              ( "pending: holiday function `"
                  <> T.unpack fnName
                  <> "` not yet registered"
              )
              (testCase treeName (pure ()))
          Just HolidaySpec {holidayFn = Nothing} ->
            ignoreTestBecause
              ( "pending: holiday `"
                  <> T.unpack fnName
                  <> "` not yet implemented"
              )
              (testCase treeName (pure ()))
          Just HolidaySpec {holidayFn = Just fn} ->
            testGroup treeName (fmap (yearCase fn) yearCols)
  _ -> testCase "<short row>" (assertFailure "holiday row too short")
  where
    yearCase ::
      (Gregorian.Year -> Maybe FixedDate) ->
      (Gregorian.Year, Int, Int) ->
      TestTree
    yearCase fn (year, mCol, dCol) =
      testCase
        (show (Gregorian.yearToInteger year))
        (checkYear fn year (cellAt mCol) (cellAt dCol))
    cellAt :: Int -> BS.ByteString
    cellAt i = case drop i row of
      x : _ -> x
      [] -> BS.empty

checkYear ::
  (Gregorian.Year -> Maybe FixedDate) ->
  Gregorian.Year ->
  BS.ByteString ->
  BS.ByteString ->
  Assertion
checkYear fn year mCell dCell =
  if stripCell mCell == BSC.pack "none"
    then case fn year of
      Nothing -> pure ()
      Just got ->
        assertFailure ("expected `none`, got " <> show got)
    else case fn year of
      Nothing ->
        assertFailure
          ( "expected "
              <> BSC.unpack (stripCell mCell)
              <> "/"
              <> BSC.unpack (stripCell dCell)
              <> ", got `none`"
          )
      Just got -> compareGregorian mCell dCell got

compareGregorian ::
  BS.ByteString -> BS.ByteString -> FixedDate -> Assertion
compareGregorian mCell dCell got = do
  expectedM <- assertEither (parseInteger mCell)
  expectedD <- assertEither (parseInteger dCell)
  let g :: Gregorian.Date
      g = fromFixed @Gregorian.Date got
      actualM :: Integer
      actualM = monthToInt (Gregorian.month g)
      actualD :: Integer
      actualD = Fin.toInteger (Gregorian.day g)
  unless (expectedM == actualM && expectedD == actualD) $
    assertFailure
      ( "expected month/day "
          <> show expectedM
          <> "/"
          <> show expectedD
          <> ", got "
          <> show actualM
          <> "/"
          <> show actualD
      )

assertEither :: Either String a -> IO a
assertEither = either assertFailure pure

monthToInt :: Gregorian.Month -> Integer
monthToInt = \case
  Gregorian.January -> 1
  Gregorian.February -> 2
  Gregorian.March -> 3
  Gregorian.April -> 4
  Gregorian.May -> 5
  Gregorian.June -> 6
  Gregorian.July -> 7
  Gregorian.August -> 8
  Gregorian.September -> 9
  Gregorian.October -> 10
  Gregorian.November -> 11
  Gregorian.December -> 12

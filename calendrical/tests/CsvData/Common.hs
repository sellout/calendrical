{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: 2026 Greg Pfeil
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
    LoadedCsv (LoadedCsv, csvLabel, csvGroups, csvDataRows),
    loadCsv,
    loadDateCsv,
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
    fileTests,
    multiFileTests,
    holidayTests,
  )
where

import safe "base" Control.Applicative (pure)
import safe "base" Control.Category ((.))
import safe "base" Control.Monad (unless)
import safe "base" Data.Bool (Bool (False, True), (&&))
import safe "base" Data.Char (Char, isSpace)
import safe "base" Data.Either (Either (Left, Right), either)
import safe "base" Data.Eq (Eq, (==))
import safe "base" Data.Foldable (foldl')
import safe "base" Data.Function (($))
import safe "base" Data.Functor (fmap, (<$>))
import safe "base" Data.Int (Int)
import safe "base" Data.Kind (Type)
import safe "base" Data.List (drop, reverse, take, zip)
import safe "base" Data.Maybe (Maybe (Just, Nothing), mapMaybe)
import safe "base" Data.Ord ((<), (<=), (>=))
import safe "base" Data.Proxy (Proxy (Proxy))
import safe "base" Data.Semigroup ((<>))
import safe "base" Data.String (String)
import safe "base" Numeric.Natural (Natural)
import safe "base" System.IO (FilePath, IO)
import safe "base" Text.Read (readMaybe)
import safe "base" Text.Show (Show, show)
import safe "bytestring" Data.ByteString qualified as BS
import safe "bytestring" Data.ByteString.Char8 qualified as BSC
import safe "bytestring" Data.ByteString.Lazy qualified as BL
import safe "calendrical" Data.Calendar
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
import safe "calendrical" Data.Calendar.Gregorian qualified as Gregorian
import "cassava" Data.Csv qualified as Csv
import safe "containers" Data.Map.Strict (Map)
import safe "containers" Data.Map.Strict qualified as Map
import safe "fin" Data.Fin (Fin)
import safe "fin" Data.Type.Nat qualified as Nat
import "tasty" Test.Tasty (TestName, TestTree, testGroup)
import "tasty-expected-failure" Test.Tasty.ExpectedFailure (ignoreTestBecause)
import "tasty-hunit" Test.Tasty.HUnit (Assertion, assertFailure, testCase)
import safe "text" Data.Text (Text)
import safe "text" Data.Text qualified as T
import "vector" Data.Vector qualified as V
import safe "base" Prelude (Integer, fromInteger, toEnum, (+))

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

-- | A two-row-header CSV with its column-group map and data rows parsed
--   out, plus a human-readable label (typically the filename).
type LoadedCsv :: Type
data LoadedCsv = LoadedCsv
  { csvLabel :: TestName,
    csvGroups :: GroupMap,
    csvDataRows :: [[BS.ByteString]]
  }

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

-- | Load a two-row-header CSV (the shape used by @dates*.csv@ and
--   @\<year\>.csv@) into a 'LoadedCsv'. The first row is parsed into a
--   group map; the second row (sub-field labels) is currently discarded.
loadDateCsv :: TestName -> FilePath -> IO LoadedCsv
loadDateCsv label path = do
  rows <- loadCsv path
  case rows of
    h1 : _ : dataRows ->
      pure (LoadedCsv label (parseHeader1 h1) dataRows)
    _ -> do
      _ <-
        assertFailure
          ("expected 2-row header + data in " <> path)
      pure (LoadedCsv label Map.empty [])

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
parseBoolean bs = case BSC.unpack $ stripCell bs of
  "f" -> Right False
  "t" -> Right True
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
parseDayOfWeek bs = case BSC.unpack $ stripCell bs of
  "Sunday" -> Right Sunday
  "Monday" -> Right Monday
  "Tuesday" -> Right Tuesday
  "Wednesday" -> Right Wednesday
  "Thursday" -> Right Thursday
  "Friday" -> Right Friday
  "Saturday" -> Right Saturday
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

-- | Build the children of a @dates*.csv@ / @\<year\>.csv@ file's
--   'TestTree', pivoted as @column-group → row@. Each implemented
--   column-group becomes a 'testGroup' with one 'testCase' per data row;
--   each not-yet-implemented column-group becomes a single
--   'ignoreTestBecause' node for the whole file (rather than one per row).
fileTests :: GroupMap -> [CalendarSpec] -> [[BS.ByteString]] -> [TestTree]
fileTests groups specs dataRows = mapMaybe (columnGroupTests dataRows groups) specs

columnGroupTests ::
  [[BS.ByteString]] ->
  GroupMap ->
  CalendarSpec ->
  Maybe TestTree
columnGroupTests dataRows groups CalendarSpec {groupName, decoder} =
  fmap build (Map.lookup groupName groups)
  where
    tname :: TestName
    tname = T.unpack groupName
    build :: (Int, Int) -> TestTree
    build (start, end) = case decoder of
      Nothing ->
        ignoreTestBecause
          ("pending: " <> tname <> " not yet implemented")
          (testCase tname (pure ()))
      Just (Decoder run) ->
        testGroup tname (fmap (rowCase run start end) dataRows)

rowCase ::
  (FixedDate -> [BS.ByteString] -> Either String ()) ->
  Int ->
  Int ->
  [BS.ByteString] ->
  TestTree
rowCase run start end row = case row of
  [] -> testCase "<empty row>" (assertFailure "empty CSV row")
  rdCell : _ -> case parseInteger rdCell of
    Left e -> testCase ("Rd <" <> BSC.unpack rdCell <> ">") (assertFailure e)
    Right rdInt ->
      testCase
        ("Rd " <> show rdInt)
        (eitherToAssertion (run (RD rdInt) (drop start (take end row))))

-- | Build a 'TestTree' for a /collection/ of two-row-header CSVs, pivoted
--   so that the top level is the column-group and one level down is the
--   file. A not-yet-implemented column-group becomes a /single/
--   'ignoreTestBecause' node for the entire collection (rather than one
--   per file or one per row).
multiFileTests :: [LoadedCsv] -> [CalendarSpec] -> [TestTree]
multiFileTests = mapMaybe . specAcrossFiles

specAcrossFiles ::
  [LoadedCsv] -> CalendarSpec -> Maybe TestTree
specAcrossFiles files CalendarSpec {groupName, decoder} =
  case foldl' addPresent [] files of
    [] -> Nothing
    present -> Just $ case decoder of
      Nothing ->
        ignoreTestBecause
          ("pending: " <> tname <> " not yet implemented")
          (testCase tname (pure ()))
      Just (Decoder run) -> testGroup tname $ perFile run <$> reverse present
  where
    tname :: TestName
    tname = T.unpack groupName
    addPresent ::
      [LoadedCsv] -> LoadedCsv -> [LoadedCsv]
    addPresent acc f =
      if Map.member groupName (csvGroups f) then f : acc else acc
    perFile ::
      (FixedDate -> [BS.ByteString] -> Either String ()) ->
      LoadedCsv ->
      TestTree
    perFile run LoadedCsv {csvLabel, csvGroups, csvDataRows} =
      case Map.lookup groupName csvGroups of
        Just (start, end) ->
          testGroup csvLabel (fmap (rowCase run start end) csvDataRows)
        Nothing -> testGroup csvLabel []

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
  expectedM <- assertEither (toEnum <$> parseInt mCell)
  expectedD <- assertEither (parseFin dCell)
  let g :: Gregorian.Date
      g = fromFixed @Gregorian.Date got
      actualM = Gregorian.month g
      actualD = Gregorian.day g
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

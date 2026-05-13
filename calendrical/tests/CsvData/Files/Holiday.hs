{-# LANGUAGE Trustworthy #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-commercial
--
-- Build a 'TestTree' for @holiday-list.csv@.
module CsvData.Files.Holiday
  ( holidayFileTests,
  )
where

import CsvData.Common (holidayTests, loadCsv, parseHolidayYearColumns)
import CsvData.Holidays (holidayMap)
import "base" Control.Applicative (pure)
import "base" Data.Function (($))
import "base" Data.Functor (fmap)
import "base" Data.List (length)
import "base" Data.Semigroup ((<>))
import "base" System.IO (FilePath, IO)
import "base" Text.Show (show)
import "tasty" Test.Tasty (TestTree, testGroup)
import "tasty-hunit" Test.Tasty.HUnit (assertFailure, testCase)

holidayFileTests :: FilePath -> FilePath -> IO TestTree
holidayFileTests label path = do
  rows <- loadCsv path
  case rows of
    header : dataRows ->
      let yearCols = parseHolidayYearColumns header
       in pure $
            testGroup
              label
              (fmap (holidayTests yearCols holidayMap) dataRows)
    _ ->
      pure $
        testCase
          label
          ( assertFailure
              ("expected header + rows, got " <> show (length rows))
          )

{-# LANGUAGE Trustworthy #-}

-- |
-- Copyright: 2026 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-commercial
--
-- Build a 'TestTree' for @holiday-list.csv@.
module CsvData.Files.Holiday
  ( holidayFileTests,
  )
where

import safe "base" Control.Applicative (pure)
import safe "base" Control.Category ((.))
import safe "base" Data.Function (($))
import safe "base" Data.Functor ((<$>))
import safe "base" Data.Semigroup ((<>))
import safe "base" System.IO (FilePath, IO)
import "tasty" Test.Tasty (TestTree, testGroup)
import "tasty-hunit" Test.Tasty.HUnit (assertFailure, testCase)
import safe "this" CsvData.Common
  ( holidayTests,
    loadCsv,
    parseHolidayYearColumns,
  )
import safe "this" CsvData.Holidays (holidayMap)

holidayFileTests :: FilePath -> FilePath -> IO TestTree
holidayFileTests label path = do
  rows <- loadCsv path
  pure case rows of
    header : dataRows ->
      testGroup label $
        holidayTests (parseHolidayYearColumns header) holidayMap <$> dataRows
    [] -> testCase label . assertFailure $ "empty holiday file at " <> path

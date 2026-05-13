{-# LANGUAGE Trustworthy #-}

-- |
-- Copyright: 2026 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-commercial
--
-- Build a 'TestTree' for one @dates*.csv@ or @\<year\>.csv@ file. Both have
-- the same two-row-header shape.
module CsvData.Files.Dates
  ( datesTests,
  )
where

import safe "base" Control.Applicative (pure)
import safe "base" Control.Category ((.))
import safe "base" Data.Function (($))
import safe "base" Data.List (length)
import safe "base" Data.Semigroup ((<>))
import safe "base" System.IO (FilePath, IO)
import safe "base" Text.Show (show)
import "tasty" Test.Tasty (TestTree, testGroup)
import "tasty-hunit" Test.Tasty.HUnit (assertFailure, testCase)
import safe "this" CsvData.Calendars (calendarSpecs)
import safe "this" CsvData.Common (fileTests, loadCsv, parseHeader1)

datesTests :: FilePath -> FilePath -> IO TestTree
datesTests label path = do
  rows <- loadCsv path
  pure case rows of
    h1 : _ : dataRows ->
      testGroup label $ fileTests (parseHeader1 h1) calendarSpecs dataRows
    _ ->
      testCase label . assertFailure $ "expected 2-row header + data, got " <> show (length rows)

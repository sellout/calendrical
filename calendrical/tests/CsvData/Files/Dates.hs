{-# LANGUAGE Trustworthy #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-commercial
--
-- Build a 'TestTree' for one @dates*.csv@ or @\<year\>.csv@ file. Both have
-- the same two-row-header shape.
module CsvData.Files.Dates
  ( datesTests,
  )
where

import CsvData.Calendars (calendarSpecs)
import CsvData.Common (loadCsv, parseHeader1, rowTests)
import "base" Control.Applicative (pure)
import "base" Data.Function (($))
import "base" Data.Functor (fmap)
import "base" Data.List (length)
import "base" Data.Semigroup ((<>))
import "base" System.IO (FilePath, IO)
import "base" Text.Show (show)
import "tasty" Test.Tasty (TestTree, testGroup)
import "tasty-hunit" Test.Tasty.HUnit (assertFailure, testCase)

datesTests :: FilePath -> FilePath -> IO TestTree
datesTests label path = do
  rows <- loadCsv path
  case rows of
    h1 : _ : dataRows ->
      pure $
        testGroup
          label
          (fmap (rowTests (parseHeader1 h1) calendarSpecs) dataRows)
    _ ->
      pure $
        testCase
          label
          ( assertFailure
              ("expected 2-row header + data, got " <> show (length rows))
          )

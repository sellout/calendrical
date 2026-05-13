{-# LANGUAGE Trustworthy #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-commercial
module Main (main) where

import "base" Control.Category ((.))
import "base" Data.Function (($))
import "base" Data.Semigroup ((<>))
import "base" Data.Traversable (traverse)
import "base" System.IO (FilePath, IO)
import "tasty" Test.Tasty
  ( Timeout (Timeout),
    defaultMain,
    localOption,
    testGroup,
  )
import "this" CsvData.Files.Dates (datesTests)
import "this" CsvData.Files.Holiday (holidayFileTests)
import "this" Paths_calendrical (getDataFileName)

-- | Per-test timeout (in microseconds). A pre-existing library bug in
--   `modularToEnum` causes some conversions to recurse infinitely; rather
--   than hanging the whole suite, treat that as a failure.
testTimeout :: Timeout
testTimeout = Timeout 2000000 "2s"

main :: IO ()
main = do
  datesTrees <- traverse (loadOne datesTests) datesFiles
  yearTrees <- traverse (loadOne datesTests) yearFiles
  holidayPath <- getDataFileName "tests/data/holiday-list.csv"
  holidayTree <- holidayFileTests "holiday-list.csv" holidayPath
  defaultMain . localOption testTimeout $
    testGroup
      "csv-data"
      [ testGroup "dates*.csv" datesTrees,
        testGroup "<year>.csv" yearTrees,
        holidayTree
      ]
  where
    loadOne ::
      (FilePath -> FilePath -> IO a) -> FilePath -> IO a
    loadOne f name = do
      p <- getDataFileName ("tests/data/" <> name)
      f name p

datesFiles :: [FilePath]
datesFiles =
  [ "dates1.csv",
    "dates2.csv",
    "dates3.csv",
    "dates4.csv",
    "dates5.csv",
    "dates6.csv",
    "dates7.csv"
  ]

yearFiles :: [FilePath]
yearFiles =
  [ "2010.csv",
    "2011.csv",
    "2012.csv",
    "2013.csv",
    "2014.csv",
    "2015.csv",
    "2016.csv",
    "2017.csv",
    "2018.csv",
    "2019.csv"
  ]

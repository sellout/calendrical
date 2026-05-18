{-# LANGUAGE Trustworthy #-}

-- |
-- Copyright: 2026 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-commercial
module Main (main) where

import "base" System.IO (IO)
import "tasty" Test.Tasty (defaultMain)
import "this" Properties.Calendars (allCalendarTests)

main :: IO ()
main = defaultMain allCalendarTests

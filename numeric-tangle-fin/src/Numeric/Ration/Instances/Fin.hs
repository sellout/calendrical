{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-commercial
module Numeric.Ration.Instances.Fin () where

import "base" Data.Ratio (Ratio)
import "base" Data.Ratio qualified as Ratio ((%))
import "fin" Data.Fin (Fin)
import "fin" Data.Type.Nat (SNatI)
import "numeric-tangle" Numeric.Ration (Ration, Result, (%))

instance (SNatI n) => Ration (Fin n) where
  type Result (Fin n) = Ratio (Fin n)
  (%) = (Ratio.%)

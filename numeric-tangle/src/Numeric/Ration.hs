{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-commercial
module Numeric.Ration
  ( Ration,
    Result,
    rationalize,
    (%),
  )
where

import "base" Data.Int (Int)
import "base" Data.Kind (Constraint, Type)
import "base" Data.Ratio (Ratio, Rational)
import "base" Data.Ratio qualified as Ratio ((%))
import "base" Foreign.C.Types (CDouble, CFloat)
import "base" Numeric.Natural (Natural)
import "base" Prelude (Double, Float, Integer, Integral, Real, toRational)
import "base" Prelude qualified as Base ((/))

-- | To approximate a numerical tower, this provides an operator that always
--   results in an exact ratio, regardless of what `Real` it’s applied to.
type Ration :: Type -> Constraint
class (Real n) => Ration n where
  type Result n
  (%) :: n -> n -> Result n

infixl 7 %

rationalize :: (Integral n, Ration n) => n -> Result n
rationalize = (% 1)

instance Ration Int where
  type Result Int = Ratio Int
  (%) = (Ratio.%)

instance Ration Integer where
  type Result Integer = Ratio Integer
  (%) = (Ratio.%)

instance Ration Natural where
  type Result Natural = Ratio Natural
  (%) = (Ratio.%)

instance (Integral n) => Ration (Ratio n) where
  type Result (Ratio n) = Ratio n
  (%) = (Base./)

instance Ration CDouble where
  type Result CDouble = Rational
  a % b = toRational a Base./ toRational b

instance Ration CFloat where
  type Result CFloat = Rational
  a % b = toRational a Base./ toRational b

instance Ration Double where
  type Result Double = Rational
  a % b = toRational a Base./ toRational b

instance Ration Float where
  type Result Float = Rational
  a % b = toRational a Base./ toRational b

{-# LANGUAGE Safe #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- This allows the deep `Fin` depths to compile.
{-# OPTIONS_GHC -freduction-depth=0 #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-commercial
module Numeric.Widen.Instances.Fin () where

import "base" Data.Int (Int, Int8)
import "base" Data.Word (Word8)
import "base" Foreign.C.Types (CChar, CSChar, CUChar)
import "base" Numeric.Natural (Natural)
import "fin" Data.Fin (Fin)
import "fin" Data.Fin qualified as Fin
import "fin" Data.Nat (Nat)
import "fin" Data.Nat qualified as Nat
import "fin" Data.Type.Nat (SNatI)
import "fin" Data.Type.Nat qualified as TNat (FromGHC)
import "fin" Data.Type.Nat.LE qualified as TNat (LE)
import "numeric-tangle" Numeric.Widen (Widen, widen)
import "base" Prelude (Integer, fromIntegral)

instance (SNatI n, TNat.LE n (TNat.FromGHC 128)) => Widen (Fin n) CChar where
  widen = fromIntegral

instance (SNatI n, TNat.LE n (TNat.FromGHC 128)) => Widen (Fin n) CSChar where
  widen = fromIntegral

instance (SNatI n, TNat.LE n (TNat.FromGHC 256)) => Widen (Fin n) CUChar where
  widen = fromIntegral

instance (SNatI n, TNat.LE n (TNat.FromGHC 128)) => Widen (Fin n) Int8 where
  widen = fromIntegral

instance (SNatI n, TNat.LE (TNat.FromGHC 128) n) => Widen Int8 (Fin n) where
  widen = fromIntegral

instance (SNatI n, TNat.LE n (TNat.FromGHC 256)) => Widen (Fin n) Word8 where
  widen = fromIntegral

instance (SNatI n, TNat.LE (TNat.FromGHC 256) n) => Widen Word8 (Fin n) where
  widen = fromIntegral

instance Widen (Fin n) Natural where
  widen = Fin.toNatural

instance Widen (Fin n) Nat where
  widen = Fin.toNat

instance Widen Nat Natural where
  widen = Nat.toNatural

instance Widen Natural Nat where
  widen = Nat.fromNatural

-- transitive instances

-- |
--
--  __FIXME__: This bound is too low, but if it’s too big, we get a stack
--             overflow.
instance
  (SNatI n, TNat.LE n (TNat.FromGHC 1024 {- 536870911 -})) =>
  Widen (Fin n) Int
  where
  widen = fromIntegral

instance (SNatI n) => Widen (Fin n) Integer where
  widen = fromIntegral

instance Widen Nat Integer where
  widen = fromIntegral

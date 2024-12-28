{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-commercial
--
-- This module can both store integral and floating-point values in a
-- mixed-radix representation, and also dynamically parse/print “regular”
-- numbers in mixed-radix formats.
--
-- The types are quite fine-grained, but at a high level, fall into these 16
-- groups. Some of the groups are compile-time precise (✔) while others require
-- at least some runtime checking (➖).
--
-- |              | integral | fixed-point | floating-point | rational |
-- | unbounded    | ✔        | ✔           | ➖              | ➖        |
-- | non-negative | ✔        | ✔           | ➖              | ➖        |
-- | bounded      | ✔        | ✔           | ➖              | ➖        |
-- | fractional   | ✔       | ✔           | ➖              | ➖        |
--
-- Floating-point types need to be bounds-checked at runtime. Rational types do
-- as well, but only upper bounds (as `Ratio Natural` is used to prevent signed
-- values).
--
-- “Bounded” means arbitrarily upper-bounded, but lower-bounded at 0.
--
-- “Fractional” means that it’s in the range (-1..1). As is hopefully obvious,
-- the only possible “fractional integral” value is 0.
--
-- __TODO__: Might be better to split the dynamic and typed bits into separate
--           modules, with the typed operations relying on some of the dynamic
--           ones.
--
-- __TODO__: Support
--          [factoradic](https://en.wikipedia.org/wiki/Factorial_number_system),
--          primoral, and other number systems where the radices follow some
--          pattern, rather than being explicitly listed.
module Numeric.MixedRadix
  ( MixedIntegral (ZeroRadix, Unbounded, IntRadix),
    MixedRadix (Integral, FinalFrac, FixedFrac, FracRadix),
    FloatFailure,
    finalFrac,
    Mixy,
    eval,
    interpret,
    render,
    toList,
    zeroIntegral,

    -- * __TODO__: exported for debugging, delete later
    dur2w3d2h5m2s,
  )
where

import "base" Control.Applicative (pure, (<*>))
import "base" Control.Category (id, (.))
import "base" Data.Bifunctor (bimap)
import "base" Data.Bitraversable (bitraverse)
import "base" Data.Bool (Bool (False, True), (&&))
import "base" Data.Either (Either (Left))
import "base" Data.Eq (Eq, (==))
import "base" Data.Function (flip, ($))
import "base" Data.Functor (fmap, (<$>))
import "base" Data.Kind (Constraint, Type)
import "base" Data.Maybe (Maybe (Just, Nothing), maybe)
import "base" Data.Monoid ((<>))
import "base" Data.Ord (Ord, Ordering (EQ), compare, (<), (<=))
import "base" Data.Proxy (Proxy (Proxy))
import "base" Data.Tuple (uncurry)
import "base" Text.Show (Show, show, showParen, showString, showsPrec)
import "fin" Data.Fin (Fin)
import "fin" Data.Fin qualified as Fin
import "fin" Data.Type.Nat (Nat, SNatI)
import "fin" Data.Type.Nat qualified as Nat
import "base" Prelude
  ( Bounded,
    Enum,
    Integer,
    Integral,
    Num,
    RealFrac,
    String,
    divMod,
    error,
    fromEnum,
    fromInteger,
    fromIntegral,
    fromRational,
    maxBound,
    minBound,
    properFraction,
    realToFrac,
    round,
    toEnum,
    toRational,
    (*),
    (+),
    (/),
  )

-- | Components are stored least-significant first.
--
--  __TODO__: Replace `Bool` with @Unbounded@/@LowerBounded@/@Bounded@, mapping
--            to `Integer`/`Natural`/`()`.
type MixedIntegral :: [Nat] -> Bool -> Type
data MixedIntegral int bounded where
  ZeroRadix :: MixedIntegral '[] 'True
  Unbounded :: Integer -> MixedIntegral '[] 'False
  IntRadix ::
    (SNatI n) => Fin n -> MixedIntegral i b -> MixedIntegral (n ': i) b

-- | The integral components are stored least-significant first, while the
--   fractional components are most-significant first.
--
--  __TODO__: Consider making @frac :: `Data.List.NonEmpty` `Nat`@.
type MixedRadix :: [Nat] -> Bool -> [Nat] -> Maybe Type -> Type
data MixedRadix int bounded frac final where
  -- | This can only be used as the least-significant component. It results in a
  --   `MixedRadix` value where the least-significant component is a
  --   floating-point value, which means it can have parts smaller than the
  --  “fixed” radix.
  FinalFrac :: (SNatI n) => final -> MixedRadix '[] 'True '[n] ('Just final)
  -- | This can only be used as the least-significant component. It results in a
  --  `MixedRadix` value with precision limited to the product of the fractional
  --   radices.
  FixedFrac :: Fin n -> MixedRadix '[] 'True '[n] 'Nothing
  -- | Intermediate fractional radices, which must be integral-valued.
  FracRadix ::
    Fin n ->
    MixedRadix '[] 'True (f ': fs) fi ->
    MixedRadix '[] 'True (n ': f ': fs) fi
  -- | The structure of the integral portion of the `MixedRadix` number.
  Integral :: MixedIntegral i b -> MixedRadix '[] 'True f fi -> MixedRadix i b f fi

-- | Remove the integral part of a `MixedRadix` number.
zeroIntegral :: MixedRadix i b f fi -> MixedRadix '[] 'True f fi
zeroIntegral = \case
  Integral _ frac -> frac
  frac@(FinalFrac {}) -> frac
  frac@(FixedFrac {}) -> frac
  frac@(FracRadix {}) -> frac

type FloatFailure :: Type
data FloatFailure
  = -- | Can’t construct a negative fractional component.
    NegativeFrac
  | -- | Can’t construct a float component larger than the radix.
    FracOverflow
  deriving stock (Enum, Eq, Ord, Show)

finalFrac ::
  forall n final.
  (SNatI n, Num final, Ord final) =>
  final ->
  Either FloatFailure (MixedRadix '[] 'True '[n] ('Just final))
finalFrac n
  | n < 0 = Left NegativeFrac
  | Nat.reflectToNum (Proxy :: Proxy n) <= n = Left FracOverflow
  | True = pure $ FinalFrac n

-- | Simple example.
dur2w3d2h5m2s ::
  MixedRadix
    '[Nat.Nat7]
    'False
    '[Nat.FromGHC 24, Nat.FromGHC 60, Nat.FromGHC 60]
    'Nothing
dur2w3d2h5m2s =
  Integral (IntRadix 3 $ Unbounded 2) . FracRadix 2 . FracRadix 5 $ FixedFrac 2

-- | Operations over all mixed-radix numbers.
--
--  __NB__: It’s tempting to switch the parameter order to `Mixy` to make it a
--          functor morphism, but the functional dependency means that
--          `TypeApplications` are easier with this order.
type Mixy :: Type -> (Type -> Constraint) -> Constraint
class Mixy m c | m -> c where
  -- | Evaluate a mixed-radix number to a type constrained by @c@.
  eval :: (c n) => m -> n

  -- | Interpret a value constrained by @c@ as a mixed-radix number. The first
  --   parameter is a scaling factor.
  interpret' :: (c n) => n -> n -> Either FloatFailure m

  -- | Generally what you want instead of `interpret'` – it defaults the scaling
  --   factor to @1@.
  interpret :: (Num n, c n) => n -> Either FloatFailure m
  interpret = interpret' 1

  -- | Produce a human-readable representation of a mixed-radix number.
  render :: m -> String

  -- | Returns the components as a list, all expanded to the same type.
  toList :: (c n) => m -> [n] -> [n]

-- Bounded

instance Bounded (MixedIntegral '[] 'True) where
  minBound = ZeroRadix
  maxBound = ZeroRadix

instance
  (SNatI i, Bounded (MixedIntegral is 'True)) =>
  Bounded (MixedIntegral ('Nat.S i ': is) 'True)
  where
  minBound = IntRadix minBound minBound
  maxBound = IntRadix maxBound maxBound

-- Enum

instance Enum (MixedIntegral '[] 'False) where
  fromEnum (Unbounded n) = fromIntegral n
  toEnum = Unbounded . fromIntegral

instance Enum (MixedIntegral '[] 'True) where
  fromEnum ZeroRadix = 0
  toEnum = \case
    0 -> ZeroRadix
    _ -> error "No MixedIntegral for this value."

instance
  (SNatI i, Enum (MixedIntegral is b)) =>
  Enum (MixedIntegral (i ': is) b)
  where
  fromEnum (IntRadix n t) =
    fromIntegral n + fromEnum t * Nat.reflectToNum (Proxy :: Proxy i)
  toEnum =
    uncurry (flip IntRadix)
      . bimap toEnum fromIntegral
      . (`divMod` Nat.reflectToNum (Proxy :: Proxy i))

-- Eq

instance Eq (MixedIntegral '[] 'False) where
  Unbounded n == Unbounded n' = n == n'

instance Eq (MixedIntegral '[] 'True) where
  ZeroRadix == ZeroRadix = True

instance (Eq (MixedIntegral is b)) => Eq (MixedIntegral (i ': is) b) where
  IntRadix n r == IntRadix n' r' = n == n' && r == r'

-- Mixy

instance Mixy (MixedIntegral '[] 'False) Integral where
  eval (Unbounded n) = fromIntegral n
  interpret' placeValue = pure . Unbounded . fromIntegral . (* placeValue)
  render (Unbounded n) = show n
  toList (Unbounded n) = (fromIntegral n :)

instance Mixy (MixedIntegral '[] 'True) Integral where
  eval ZeroRadix = 0
  interpret' _ = interpret
  interpret = \case
    0 -> pure ZeroRadix
    _ -> Left FracOverflow -- FIXME: Give new error case?
  render ZeroRadix = ""
  toList ZeroRadix = id

instance
  (SNatI a, Mixy (MixedIntegral as b) Integral) =>
  Mixy (MixedIntegral (a ': as) b) Integral
  where
  eval (IntRadix n prev) =
    fromIntegral n + eval prev * Nat.reflectToNum (Proxy :: Proxy a)
  interpret' placeValue =
    let radix = Nat.reflectToNum (Proxy :: Proxy a)
     in fmap (uncurry $ flip IntRadix)
          . bitraverse
            (interpret' $ placeValue * radix)
            (maybe (Left FracOverflow) pure . Fin.fromNat . fromIntegral)
          . (`divMod` radix)
          . (* placeValue)
  render (IntRadix n prev) = render prev <> ", " <> show n
  toList (IntRadix n r) = toList r . (fromIntegral n :)

instance (SNatI a, Show n, RealFrac n) => Mixy (MixedRadix '[] 'True '[a] ('Just n)) RealFrac where
  eval = \case
    FinalFrac n -> realToFrac n
    Integral ZeroRadix m -> eval m
  interpret' placeValue value =
    case properFraction value of
      (0, f) -> finalFrac . realToFrac $ f / placeValue
      (_ :: Integer, _) -> Left FracOverflow
  render = \case
    FinalFrac n -> show n
    Integral ZeroRadix m -> render m
  toList = \case
    FinalFrac n -> (fromRational (toRational n) :)
    Integral ZeroRadix m -> toList m

instance (SNatI a) => Mixy (MixedRadix '[] 'True '[a] 'Nothing) RealFrac where
  eval = \case
    FixedFrac n -> fromIntegral n
    Integral ZeroRadix m -> eval m
  interpret' placeValue value =
    case properFraction value of
      (0, f) ->
        let v = f / placeValue
         in if Nat.reflectToNum (Proxy :: Proxy a) <= v
              then Left NegativeFrac
              else pure . FixedFrac $ round v
      (_ :: Integer, _) -> Left FracOverflow
  render = \case
    FixedFrac n -> show n
    Integral ZeroRadix m -> render m
  toList = \case
    FixedFrac n -> (fromIntegral n :)
    Integral ZeroRadix m -> toList m

instance
  (SNatI a, Mixy (MixedRadix '[] 'True (a' ': as) fi) RealFrac) =>
  Mixy (MixedRadix '[] 'True (a ': a' ': as) fi) RealFrac
  where
  eval = \case
    FracRadix n prev ->
      fromIntegral n + eval prev / Nat.reflectToNum (Proxy :: Proxy a)
    Integral ZeroRadix m -> eval m
  interpret' placeValue value = case properFraction value of
    (0, f) ->
      let radix = Nat.reflectToNum (Proxy :: Proxy a)
          v = f / placeValue
       in if radix <= v
            then Left NegativeFrac
            else
              fmap (uncurry FracRadix)
                . bitraverse pure (interpret' $ placeValue / radix)
                $ properFraction v
    (_ :: Integer, _) -> Left FracOverflow
  render = \case
    FracRadix n prev -> show n <> ", " <> render prev
    Integral ZeroRadix m -> render m
  toList = \case
    FracRadix n r -> (fromIntegral n :) . toList r
    Integral ZeroRadix m -> toList m

-- The next two instances are identical, but specialized this way to avoid
-- overlapping instances.

instance
  ( Mixy (MixedIntegral '[] 'False) Integral,
    Mixy (MixedRadix '[] 'True f fi) RealFrac
  ) =>
  Mixy (MixedRadix '[] 'False f fi) RealFrac
  where
  eval (Integral int prev) = fromInteger (eval int) + eval prev
  interpret' placeValue value =
    let (i, f) = properFraction @_ @Integer value
     in Integral <$> interpret' (round placeValue) i <*> interpret' placeValue f
  render (Integral int prev) = render int <> "; " <> render prev
  toList (Integral int prev) meh = fmap fromInteger (toList int []) <> toList prev meh

instance
  ( Mixy (MixedIntegral (i ': is) b) Integral,
    Mixy (MixedRadix '[] 'True f fi) RealFrac
  ) =>
  Mixy (MixedRadix (i ': is) b f fi) RealFrac
  where
  eval (Integral int prev) = fromInteger (eval int) + eval prev
  interpret' placeValue value =
    let (i, f) = properFraction @_ @Integer value
     in Integral <$> interpret' (round placeValue) i <*> interpret' placeValue f
  render (Integral int prev) = render int <> "; " <> render prev
  toList (Integral int prev) meh = fmap fromInteger (toList int []) <> toList prev meh

-- Ord

instance Ord (MixedIntegral '[] 'False) where
  compare (Unbounded n) (Unbounded n') = compare n n'

instance Ord (MixedIntegral '[] 'True) where
  compare ZeroRadix ZeroRadix = EQ

instance (Ord (MixedIntegral is b)) => Ord (MixedIntegral (i ': is) b) where
  compare (IntRadix n r) (IntRadix n' r') = compare r r' <> compare n n'

-- Show

instance Show (MixedIntegral i b) where
  showsPrec p = \case
    Unbounded n ->
      showParen (nextPrec <= p) $ showString "Unbounded " . showsPrec nextPrec n
    ZeroRadix -> showString "ZeroRadix"
    IntRadix n r ->
      showParen (nextPrec <= p) $
        showString "IntRadix "
          . showsPrec nextPrec n
          . showString " "
          . showsPrec nextPrec r
    where
      nextPrec = 11

instance (Show final) => Show (MixedRadix '[] 'True '[n] ('Just final)) where
  showsPrec p =
    showParen (nextPrec <= p) . \case
      FinalFrac n -> showString "FinalFrac " . showsPrec nextPrec n
      Integral ZeroRadix m ->
        showString "Integral ZeroRadix " . showsPrec nextPrec m
    where
      nextPrec = 11

instance Show (MixedRadix '[] 'True '[n] 'Nothing) where
  showsPrec p =
    showParen (nextPrec <= p) . \case
      FixedFrac n -> showString "FixedFrac " . showsPrec nextPrec n
      Integral ZeroRadix m ->
        showString "Integral ZeroRadix " . showsPrec nextPrec m
    where
      nextPrec = 11

instance
  (Show (MixedRadix '[] 'True (a' ': as) fi)) =>
  Show (MixedRadix '[] 'True (a ': a' ': as) fi)
  where
  showsPrec p =
    showParen (nextPrec <= p) . \case
      FracRadix n r ->
        showString "FracRadix "
          . showsPrec nextPrec n
          . showString " "
          . showsPrec nextPrec r
      Integral ZeroRadix m ->
        showString "Integral ZeroRadix " . showsPrec nextPrec m
    where
      nextPrec = 11

instance
  (Show (MixedIntegral '[] 'False), Show (MixedRadix '[] 'True f fi)) =>
  Show (MixedRadix '[] 'False f fi)
  where
  showsPrec p (Integral i f) =
    showParen (nextPrec <= p) $
      showString "Integral "
        . showsPrec nextPrec i
        . showString " "
        . showsPrec nextPrec f
    where
      nextPrec = 11

instance
  (Show (MixedIntegral (i ': is) b), Show (MixedRadix '[] 'True f fi)) =>
  Show (MixedRadix (i ': is) b f fi)
  where
  showsPrec p (Integral i f) =
    showParen (nextPrec <= p) $
      showString "Integral "
        . showsPrec nextPrec i
        . showString " "
        . showsPrec nextPrec f
    where
      nextPrec = 11

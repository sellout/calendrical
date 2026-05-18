{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: 2026 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-commercial
--
-- Class-generic Hedgehog property bundles for the calendar classes.
module Properties.Class
  ( calendarTests,
    linearTests,
    cyclicTests,
    genFixedDate,
  )
where

import "base" Control.Applicative ((<*>))
import "base" Data.Eq (Eq)
import "base" Data.Foldable (for_)
import "base" Data.Function (($), (.))
import "base" Data.Functor ((<$>))
import "base" Data.Int (Int)
import "base" Data.List qualified as List
import "base" Data.List.NonEmpty qualified as NonEmpty
import "base" Data.Maybe (Maybe (Just, Nothing))
import "base" Data.Ord ((<), (<=))
import "base" Data.Semigroup ((<>))
import "base" Text.Show (Show)
import "calendrical" Data.Calendar
  ( Calendar,
    CyclicCalendar,
    FixedDate (RD),
    LinearCalendar,
    fixedFrom,
    fixedsFrom,
    fromFixed,
    offset,
    onOrBefore,
  )
import "calendrical" Data.Calendar.Types (NonnegativeInteger)
import "hedgehog" Hedgehog (Gen, assert, forAll, property, (===))
import "hedgehog" Hedgehog.Gen qualified as Gen
import "hedgehog" Hedgehog.Range qualified as Range
import "tasty" Test.Tasty (TestTree, testGroup)
import "tasty-hedgehog" Test.Tasty.Hedgehog (testProperty)
import "base" Prelude (Integer, length, mod, toInteger, (-))

-- | Generates `FixedDate`s in a wide band around @`RD` 0@ (≈ ±10⁴ years).
--   Wide enough to cross many cycles for the cyclic calendars, narrow enough
--   that integer arithmetic in the calendars stays well‑behaved.
genFixedDate :: Gen FixedDate
genFixedDate = RD <$> Gen.integral (Range.linearFrom 0 (-3_650_000) 3_650_000)

-- | Window into a cycle: an offset @n@ to skip and a length @m@ to inspect.
genWindow :: Gen (Int, Int)
genWindow =
  (,)
    <$> Gen.int (Range.linear 0 100)
    <*> Gen.int (Range.linear 1 20)

-- | Properties every `Calendar` instance must satisfy: `fromFixed` is total,
--   and every element of any prefix of `fixedsFrom d` projects back to @d@.
calendarTests ::
  forall a proxy.
  (Calendar a, Show a, Eq a) =>
  proxy a ->
  TestTree
calendarTests _ =
  testGroup
    "Calendar"
    [ testProperty "fromFixed total ∧ fixedsFrom members project back to date" $
        property do
          rd <- forAll genFixedDate
          (n, m) <- forAll genWindow
          let d :: a
              d = fromFixed rd
              sample =
                List.take m . List.drop n . NonEmpty.toList $ fixedsFrom d
          for_ sample $ \rd' -> fromFixed rd' === d
    ]

-- | Properties every `LinearCalendar` instance must satisfy: `fromFixed` and
--   `fixedFrom` are mutually inverse.
linearTests ::
  forall a proxy.
  (LinearCalendar a, Show a) =>
  proxy a ->
  TestTree
linearTests _ =
  testGroup
    "LinearCalendar"
    [ testProperty "fixedFrom . fromFixed = id on FixedDate" $ property do
        rd <- forAll genFixedDate
        fixedFrom (fromFixed @a rd) === rd,
      testProperty "fromFixed . fixedFrom = id on date" $ property do
        rd <- forAll genFixedDate
        let d = fromFixed @a rd
        fromFixed (fixedFrom d) === d
    ]

-- | Properties every `CyclicCalendar` instance must satisfy. Caller supplies
--   the cycle length and, optionally, an @ordinal@ function paired with the
--   calendar’s epoch as a `FixedDate` (used to verify
--   @ordinal d ≡ (rd − epoch) `mod` cycleLength@).
cyclicTests ::
  forall a proxy.
  (CyclicCalendar a, Show a, Eq a) =>
  proxy a ->
  -- | cycle length
  Integer ->
  -- | optional @(ordinal, epoch)@
  Maybe (a -> NonnegativeInteger, FixedDate) ->
  TestTree
cyclicTests _ cycleLen mOrdinal =
  testGroup "CyclicCalendar" $
    [ testProperty "onOrBefore d rd ≤ rd, gap < cycleLength, projects to d" $
        property do
          rd <- forAll genFixedDate
          let d = fromFixed @a rd
              rd' = d `onOrBefore` rd
          assert (rd' <= rd)
          assert (offset rd - offset rd' < cycleLen)
          fromFixed rd' === d,
      testProperty
        "fixedsFrom prefix has requested length and is cycle-aligned"
        $ property do
          rd <- forAll genFixedDate
          (n, m) <- forAll genWindow
          let d = fromFixed @a rd
              ne = fixedsFrom d
              origin = NonEmpty.head ne
              sample = List.take m . List.drop n $ NonEmpty.toList ne
          length sample === m
          -- Every element is at a whole-cycle offset from the origin. We don't
          -- require the prefix to be sorted, since calendars are free to list
          -- occurrences in any order (e.g. Mayan.Haab interleaves backward and
          -- forward from `origin`).
          for_ sample $ \rd' ->
            (offset rd' - offset origin) `mod` cycleLen === 0
    ]
      <> case mOrdinal of
        Nothing -> []
        Just (ordinal, ep) ->
          [ testProperty
              "ordinal d ∈ [0, cycleLength) ∧ matches (rd − epoch) mod cycle"
              $ property do
                rd <- forAll genFixedDate
                let d = fromFixed @a rd
                    n = toInteger (ordinal d)
                assert (n < cycleLen)
                n === (offset rd - offset ep) `mod` cycleLen
          ]

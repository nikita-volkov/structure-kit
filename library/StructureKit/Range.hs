{-# LANGUAGE StrictData #-}

module StructureKit.Range
  ( -- * --
    Range (..),

    -- ** --
    toPredicate,

    -- ** --
    isEmpty,
  )
where

import StructureKit.Prelude

-- * --

data Range a
  = Range
      -- | Equal or larger than.
      a
      -- | Smaller than.
      a

-- | Implements intersection.
instance (Ord a) => Semigroup (Range a) where
  Range lMin lMax <> Range rMin rMax =
    Range (max lMin rMin) (min lMax rMax)

instance (Bounded a, Ord a) => Monoid (Range a) where
  mempty =
    Range minBound maxBound

-- * --

-- |
-- Check whether the range contains the element.
toPredicate :: (Ord a) => Range a -> a -> Bool
toPredicate (Range min max) a =
  a >= min && a < max

-- * --

-- |
-- Checks whether the range includes any elements at all.
isEmpty :: (Ord a) => Range a -> Bool
isEmpty (Range min max) =
  min >= max

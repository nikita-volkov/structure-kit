{-# LANGUAGE StrictData #-}

module StructureKit.OpenRange
  ( Range (..),

    -- * Execution
    toPredicate,
    isEmpty,
  )
where

import StructureKit.Prelude hiding (intersection, union)

-- * --

data Range a = Range
  { -- | Equal or larger than.
    from :: Maybe a,
    -- | Smaller than.
    upto :: Maybe a
  }
  deriving (Show, Eq)

-- | Implements intersection.
instance (Ord a) => Semigroup (Range a) where
  Range lFrom lUpTo <> Range rFrom rUpTo =
    Range
      { from =
          case lFrom of
            Nothing -> rFrom
            Just lFrom -> case rFrom of
              Nothing -> Just lFrom
              Just rFrom -> Just (max lFrom rFrom),
        upto =
          case lUpTo of
            Nothing -> rUpTo
            Just lUpTo -> case rUpTo of
              Nothing -> Just lUpTo
              Just rUpTo -> Just (min lUpTo rUpTo)
      }

instance (Ord a) => Monoid (Range a) where
  mempty =
    Range Nothing Nothing

-- * Execution

-- |
-- Check whether the range contains the element.
toPredicate :: (Ord a) => Range a -> a -> Bool
toPredicate (Range from upto) a =
  maybe True (a >=) from && maybe True (a <) upto

-- |
-- Checks whether the range includes any elements at all.
isEmpty :: (Ord a) => Range a -> Bool
isEmpty (Range from upto) =
  from >= upto

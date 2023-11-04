{-# LANGUAGE StrictData #-}

module StructureKit.OpenRange
  ( OpenRange (..),

    -- * Execution
    toPredicate,
    isEmpty,
  )
where

import StructureKit.Prelude hiding (intersection, union)

-- * --

data OpenRange a = OpenRange
  { -- | Equal or larger than.
    from :: Maybe a,
    -- | Smaller than.
    upto :: Maybe a
  }
  deriving (Show, Eq)

-- | Implements intersection.
instance (Ord a) => Semigroup (OpenRange a) where
  OpenRange lFrom lUpTo <> OpenRange rFrom rUpTo =
    OpenRange
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

instance (Ord a) => Monoid (OpenRange a) where
  mempty =
    OpenRange Nothing Nothing

-- * Execution

-- |
-- Check whether the range contains the element.
toPredicate :: (Ord a) => OpenRange a -> a -> Bool
toPredicate (OpenRange from upto) a =
  maybe True (a >=) from && maybe True (a <) upto

-- |
-- Checks whether the range includes any elements at all.
isEmpty :: (Ord a) => OpenRange a -> Bool
isEmpty (OpenRange from upto) =
  from >= upto

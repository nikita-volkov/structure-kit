module StructureKit.BalancedList
  ( -- * --
    BalancedList,

    -- * --
    map,
    filter,
  )
where

import StrictList (List)
import qualified StrictList
import StructureKit.Prelude hiding (empty, filter, map)

-- * --

data BalancedList a
  = BalancedList
      !Bool
      -- ^
      -- Whether it is currently reversed.
      --
      -- This is useful because many list functions work better,
      -- when they produce in reversed order.
      --
      -- Thus it lets the next function to be called on it
      -- decide how to process this data.
      !(List a)

instance Functor BalancedList where
  fmap = map

-- * --

-- |
-- Apply a transformation to internal list,
-- which reverses its order.
mapReverseList :: (List a -> List b) -> BalancedList a -> BalancedList b
mapReverseList f (BalancedList reversed list) =
  BalancedList (not reversed) (f list)

-- * --

map :: (a -> b) -> BalancedList a -> BalancedList b
map mapper =
  mapReverseList (StrictList.mapReversed mapper)

filter :: (a -> Bool) -> BalancedList a -> BalancedList a
filter predicate =
  mapReverseList (StrictList.filterReversed predicate)

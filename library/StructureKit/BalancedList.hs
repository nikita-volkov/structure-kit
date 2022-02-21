module StructureKit.BalancedList
  ( -- *
    BalancedList,

    -- **
    filter,
  )
where

import qualified StrictList
import StructureKit.Prelude hiding (empty, filter)

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
      !(StrictList.List a)

-- **

filter :: (a -> Bool) -> BalancedList a -> BalancedList a
filter predicate (BalancedList reversed list) =
  BalancedList
    (not reversed)
    (StrictList.filterReversed predicate list)

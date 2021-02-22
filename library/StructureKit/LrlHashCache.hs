{-|
Least recently looked-up cache.
-}
module StructureKit.LrlHashCache
(
  LrlHashCache,
  empty,
  lookup,
  insert,
)
where

import StructureKit.Prelude hiding (lookup, insert, empty)
import qualified StructureKit.LookupOrderedHashMap as LookupOrderedHashMap


data LrlHashCache k v =
  LrlHashCache
    Int
    {-^ Slots occupied. -}
    Int
    {-^ Slots available. -}
    (LookupOrderedHashMap.LookupOrderedHashMap k v)

empty ::
  {-| Maximum amount of entries to store at one moment.
      After it\'s reached the least recently used entry will
      be discarded on each insert. -}
  Int ->
  LrlHashCache k v
empty avail =
  LrlHashCache 0 avail LookupOrderedHashMap.empty

lookup :: (Hashable k, Eq k) => k -> LrlHashCache k v -> (Maybe v, LrlHashCache k v)
lookup key (LrlHashCache occupied avail map) =
  LookupOrderedHashMap.lookup key map & second
    if avail > 0
      then LrlHashCache (succ occupied) (pred avail)
      else LrlHashCache occupied avail . snd . LookupOrderedHashMap.evict

insert :: (Hashable k, Eq k) => k -> v -> LrlHashCache k v -> (Maybe v, LrlHashCache k v)
insert key value (LrlHashCache occupied avail map) =
  LookupOrderedHashMap.insert key value map & second (LrlHashCache occupied avail)

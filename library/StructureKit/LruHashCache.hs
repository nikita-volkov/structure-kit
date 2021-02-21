module StructureKit.LruHashCache
(
  LruHashCache,
  lookup,
  insert,
)
where

import StructureKit.Prelude hiding (lookup, insert)
import qualified StructureKit.TouchOrderedHashMap as TouchOrderedHashMap


data LruHashCache k v =
  LruHashCache
    Int
    {-^ Slots occupied. -}
    Int
    {-^ Slots available. -}
    (TouchOrderedHashMap.TouchOrderedHashMap k v)

lookup :: (Hashable k, Eq k) => k -> LruHashCache k v -> (Maybe v, LruHashCache k v)
lookup key (LruHashCache occupied avail map) =
  TouchOrderedHashMap.lookup key map & second (LruHashCache occupied avail)

insert :: (Hashable k, Eq k) => k -> v -> LruHashCache k v -> (Maybe (k, v), LruHashCache k v)
insert key value (LruHashCache occupied avail map) =
  TouchOrderedHashMap.insert key value map & \(new, map) ->
    case new of
      True ->
        case avail > 0 of
          True ->
            (Nothing, LruHashCache (succ occupied) (pred avail) map)
          False ->
            TouchOrderedHashMap.evict map
              & second (LruHashCache occupied avail)
      False ->
        (Nothing, LruHashCache occupied avail map)

module StructureKit.LruHashCache
(
  LruHashCache,
  empty,
  lookup,
  insert,
)
where

import StructureKit.Prelude hiding (lookup, insert, empty)
import qualified StructureKit.TouchOrderedHashMap as TouchOrderedHashMap


data LruHashCache k v =
  LruHashCache
    Int
    {-^ Slots occupied. -}
    Int
    {-^ Slots available. -}
    (TouchOrderedHashMap.TouchOrderedHashMap k v)

empty ::
  {-| Maximum amount of entries to store at one moment.
      After it\'s reached the least recently used entry will
      be discarded on each insert. -}
  Int ->
  LruHashCache k v
empty avail =
  LruHashCache 0 avail TouchOrderedHashMap.empty

lookup :: (Hashable k, Eq k) => k -> LruHashCache k v -> (Maybe v, LruHashCache k v)
lookup key (LruHashCache occupied avail map) =
  TouchOrderedHashMap.lookup key map & second (LruHashCache occupied avail)

insert :: (Hashable k, Eq k) => k -> v -> LruHashCache k v -> (Maybe (k, v), LruHashCache k v)
insert key value (LruHashCache occupied avail map) =
  TouchOrderedHashMap.insert key value map & \(valueReplaced, map) ->
    {- FIXME: Looks like there's a bug hiding here. -}
    case valueReplaced of
      Just _ ->
        case avail > 0 of
          True ->
            (Nothing, LruHashCache (succ occupied) (pred avail) map)
          False ->
            TouchOrderedHashMap.evict map
              & second (LruHashCache occupied avail)
      Nothing ->
        (Nothing, LruHashCache occupied avail map)

module StructureKit.LruHashCache
(
  LruHashCache,
  lookup,
)
where

import StructureKit.Prelude hiding (lookup)
import qualified StructureKit.TouchOrderedHashMap as TouchOrderedHashMap


data LruHashCache k v =
  LruHashCache
    Int
    {-^ Max size. -}
    (TouchOrderedHashMap.TouchOrderedHashMap k v)

lookup :: (Hashable k, Eq k) => k -> LruHashCache k v -> (Maybe v, LruHashCache k v)
lookup key (LruHashCache size map) =
  TouchOrderedHashMap.lookup key map & second (LruHashCache size)

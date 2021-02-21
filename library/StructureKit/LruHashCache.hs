module StructureKit.LruHashCache
(
  LruHashCache,
  lookup,
)
where

import StructureKit.Prelude hiding (lookup)
import qualified StructureKit.LookupOrderedHashMap as LookupOrderedHashMap


data LruHashCache k v =
  LruHashCache
    Int
    {-^ Max size. -}
    (LookupOrderedHashMap.LookupOrderedHashMap k v)

lookup :: (Hashable k, Eq k) => k -> LruHashCache k v -> (Maybe v, LruHashCache k v)
lookup key (LruHashCache size map) =
  error "TODO"

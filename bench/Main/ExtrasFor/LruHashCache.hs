module Main.ExtrasFor.LruHashCache where

import StructureKit.LruHashCache
import Prelude hiding (empty, insert, lookup)

insertMany :: (Hashable k, Eq k) => [(k, v)] -> LruHashCache k v -> ([(k, v)], LruHashCache k v)
insertMany =
  \inserts lhc -> foldr step end inserts lhc []
  where
    step (k, v) next !lhc !evictions =
      case insert k v lhc of
        (eviction, lhc) ->
          let evictions' = case eviction of
                Just eviction -> eviction : evictions
                Nothing -> evictions
           in next lhc evictions'
    end lhc evictions =
      (reverse evictions, lhc)

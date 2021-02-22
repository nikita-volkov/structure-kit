module StructureKit.ByteArrayCache
(
  ByteArrayCache,
)
where

import StructureKit.Prelude
import qualified StructureKit.ByteArrayMap as ByteArrayMap
import qualified StructureKit.ByteArrayCacheEntry as ByteArrayCacheEntry


data ByteArrayCache a =
  ByteArrayCache
    (Deque ByteString)
    (ByteArrayMap.ByteArrayMap (ByteArrayCacheEntry.ByteArrayCacheEntry a))

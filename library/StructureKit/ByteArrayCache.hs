module StructureKit.ByteArrayCache
  ( ByteArrayCache,
  )
where

import qualified StructureKit.ByteArrayCacheEntry as ByteArrayCacheEntry
import qualified StructureKit.ByteArrayMap as ByteArrayMap
import StructureKit.Prelude

data ByteArrayCache a
  = ByteArrayCache
      (Deque ByteString)
      (ByteArrayMap.ByteArrayMap (ByteArrayCacheEntry.ByteArrayCacheEntry a))

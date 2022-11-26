module StructureKit.ByteArrayCache
  ( ByteArrayCache,
  )
where

import StructureKit.ByteArrayCacheEntry qualified as ByteArrayCacheEntry
import StructureKit.ByteArrayMap qualified as ByteArrayMap
import StructureKit.Prelude

data ByteArrayCache a
  = ByteArrayCache
      (Deque ByteString)
      (ByteArrayMap.ByteArrayMap (ByteArrayCacheEntry.ByteArrayCacheEntry a))

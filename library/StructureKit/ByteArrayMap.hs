module StructureKit.ByteArrayMap
(
  ByteArrayMap,
  here,
  mapAt,
  consumeLongestPrefix,
)
where

import StructureKit.Prelude
import qualified StructureKit.By8Bits as By8Bits
import qualified Data.ByteString as ByteString


{-|
Map indexed by a sequence of bytes. Be it ByteArray or ByteString.
-}
data ByteArrayMap a =
  ByteArrayMap
    (Maybe a)
    {-^ Possible value at zero prefix. -}
    (By8Bits.By8Bits (ByteArrayMap a))
    {-^ Next trees indexed by first byte of the suffix. -}

mapAt :: (a -> a) -> ByteString -> ByteArrayMap a -> ByteArrayMap a
mapAt =
  error "TODO"

here :: ByteArrayMap a -> Maybe a
here =
  error "TODO"

{-|
Consume as many bytes as possible until the tree ends,
returning the unconsumed bytes.
-}
consumeLongestPrefix :: ByteString -> ByteArrayMap a -> (ByteString, ByteArrayMap a)
consumeLongestPrefix key (ByteArrayMap posValue byteMap) =
  ByteString.uncons key & \case
    Just (byte, nextKey) ->
      case By8Bits.lookup (fromIntegral byte) byteMap of
        Just nextModel -> consumeLongestPrefix nextKey nextModel
        Nothing -> (key, ByteArrayMap posValue byteMap)
    Nothing ->
      (mempty, ByteArrayMap posValue byteMap)

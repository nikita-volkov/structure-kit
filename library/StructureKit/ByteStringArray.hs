module StructureKit.ByteStringArray
(
  ByteStringArray,
  getByteStringMonolith,
  foldr,
)
where

import StructureKit.Prelude hiding (lookup, insert, empty, foldr)
import qualified Data.Vector.Unboxed as UVec
import qualified Data.ByteString as ByteString


{-|
Efficient representation of a vector of byte arrays.

Implemented as a view on a single byte array,
which knows where to split it into pieces.
-}
data ByteStringArray =
  ByteStringArray {
    boundaryVec :: UVec.Vector Int,
    byteString :: ByteString
  }

{-|
Get the underlying monolith vector.
-}
getByteStringMonolith :: ByteStringArray -> ByteString
getByteStringMonolith = byteString

foldr :: (ByteString -> a -> a) -> a -> ByteStringArray -> a
foldr step start (ByteStringArray boundaryVec monolith) =
  UVec.foldr boundaryVecStep boundaryVecFinalize boundaryVec monolith start
  where
    length = ByteString.length monolith
    boundaryVecStep boundary next remainder acc =
      ByteString.splitAt boundary remainder & \(chunk, newRemainder) ->
        next newRemainder (step chunk acc)
    boundaryVecFinalize remainder acc =
      if ByteString.null remainder
        then acc
        else step remainder acc

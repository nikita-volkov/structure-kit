module StructureKit.ByteArrayVector
(
  ByteArrayVector,
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
data ByteArrayVector =
  ByteArrayVector {
    boundaryVec :: UVec.Vector Int,
    byteString :: ByteString
  }

{-|
Get the underlying monolith vector.
-}
getByteStringMonolith :: ByteArrayVector -> ByteString
getByteStringMonolith = byteString

foldr :: (ByteString -> a -> a) -> a -> ByteArrayVector -> a
foldr step start (ByteArrayVector boundaryVec monolith) =
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

{-|
Set of 6-bit values represented with a 64-bit word.
-}
module StructureKit.Bits64
(
  Bits64,
  empty,
  singleton,
  lookup,
  insert,
)
where

import StructureKit.Prelude hiding (empty, lookup, adjust, insert)


newtype Bits64 =
  Bits64 Int64

empty :: Bits64
empty =
  Bits64 0

singleton :: Int -> Bits64
singleton value =
  Bits64 (bit value)

lookup :: Int -> Bits64 -> Maybe Int
lookup value (Bits64 word) =
  if value == 0
    then if word .&. 1 /= 0
      then Just 0
      else Nothing
    else let
      bitAtValue = bit value
      isPopulated = bitAtValue .&. word /= 0
      in if isPopulated
        then Just (popCount (word .&. (bitAtValue - 1)))
        else Nothing

insert :: Int -> Bits64 -> (Int, Maybe Bits64)
insert value (Bits64 word) =
  let
    bitAtValue = bit value
    newWord = word .|. bitAtValue
    newBitSet = if newWord == word then Nothing else Just (Bits64 newWord)
    in (fromIntegral bitAtValue, newBitSet)

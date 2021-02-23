module StructureKit.By6Bits
(
  By6Bits,
  empty,
  singleton,
  lookup,
  insert,
)
where

import StructureKit.Prelude hiding (empty, lookup, insert)
import qualified PrimitiveExtras.SmallArray as SmallArray
import qualified StructureKit.Prelude as Prelude
import qualified StructureKit.Bits64 as Bits64


{-|
Map indexed with 6 bits.
-}
data By6Bits a =
  By6Bits Bits64.Bits64 (SmallArray a)

empty :: By6Bits a
empty =
  By6Bits Bits64.empty Prelude.empty

{-|
An array with a single element at the specified index.
-}
singleton :: Int -> a -> By6Bits a
singleton key a = 
  let
    bitSet = Bits64.singleton key
    array = runST (newSmallArray 1 a >>= unsafeFreezeSmallArray)
    in By6Bits bitSet array

lookup :: Int -> By6Bits a -> Maybe a
lookup key (By6Bits bitSet array) =
  Bits64.lookup key bitSet
    & fmap (\index -> indexSmallArray array index)

insert :: Int -> a -> By6Bits a -> (Maybe a, By6Bits a)
insert key value (By6Bits bitSet array) =
  Bits64.insert key bitSet & \case
    (index, newBitSetMaybe) -> case newBitSetMaybe of
      Just newBitSet ->
        (Nothing,
          By6Bits newBitSet (SmallArray.set index value array))
      Nothing ->
        (Just (indexSmallArray array index),
          By6Bits bitSet (SmallArray.insert index value array))

module StructureKit.By6Bits
(
  By6Bits,
  empty,
  singleton,
  lookup,
  insert,
  adjust,
  revision,
  foldrWithKey,
  toList,
)
where

import StructureKit.Prelude hiding (empty, lookup, insert, toList)
import qualified StructureKit.Util.SmallArray as SmallArray
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
          By6Bits newBitSet (SmallArray.insert index value array))
      Nothing ->
        (Just (indexSmallArray array index),
          By6Bits bitSet (SmallArray.set index value array))

{-# INLINE adjust #-}
adjust :: (a -> a) -> Int -> By6Bits a -> By6Bits a
adjust fn key (By6Bits bitSet array) =
  case Bits64.lookup key bitSet of
    Just index ->
      By6Bits bitSet
        (SmallArray.unsafeAdjust fn index array)
    Nothing ->
      By6Bits bitSet array

{-|
Very much like @alterF@ of the \"containers\" package
with two differences:

- For better performance @(Maybe a -> f (Maybe a))@ is replaced with
  the following two continuations: @f (Maybe a)@ and @(a -> f (Maybe a))@.
- The result is packed in @Maybe@ to inform you whether the map
  has become empty.
  This is useful for working with tries, since it can be used as
  a single to remove from a wrapping container.
-}
{-# INLINE revision #-}
revision :: Functor f => Int -> f (Maybe a) -> (a -> f (Maybe a)) -> By6Bits a -> f (Maybe (By6Bits a))
revision key onMissing onPresent (By6Bits bitSet array) =
  Bits64.revision key
    (\index ->
      Compose (fmap
        (\case
          Just value ->
            (SmallArray.insert index value array, True)
          Nothing ->
            (array, False))
        onMissing))
    (\index ->
      Compose (fmap
        (\case
          Just value ->
            (SmallArray.set index value array, True)
          Nothing ->
            (array, False))
        (onPresent
          (indexSmallArray array index))))
    bitSet
    & getCompose
    & fmap (\(array, bitSetMaybe) ->
        fmap (\bitSet -> By6Bits bitSet array)
          bitSetMaybe)

{-# INLINE foldrWithKey #-}
foldrWithKey :: (Int -> a -> b -> b) -> b -> By6Bits a -> b
foldrWithKey step end (By6Bits bits array) =
  loop 0 0
  where
    loop key arrayIndex =
      if key < 64
        then if Bits64.member key bits
          then case indexSmallArray array arrayIndex of
            element -> step key element (loop (succ key) (succ arrayIndex))
          else loop (succ key) arrayIndex
        else end

toList :: By6Bits a -> [(Int, a)]
toList =
  foldrWithKey (\k v -> (:) (k, v)) []

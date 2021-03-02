module StructureKit.By8Bits
(
  By8Bits,
  empty,
  lookup,
  insert,
  foldrWithKey,
)
where

import StructureKit.Prelude hiding (empty, lookup, insert)
import qualified StructureKit.By6Bits as By6Bits


{-|
Map indexed with 8 bits.
-}
data By8Bits a =
  By8Bits (By6Bits.By6Bits a) (By6Bits.By6Bits a) (By6Bits.By6Bits a) (By6Bits.By6Bits a)

empty :: By8Bits a
empty =
  By8Bits By6Bits.empty By6Bits.empty By6Bits.empty By6Bits.empty

lookup :: Int -> By8Bits a -> Maybe a
lookup key (By8Bits a b c d) =
  By6Bits.lookup key
    if key < 128
      then if key < 64
        then a
        else b
      else if key < 192
        then c
        else d

insert :: Int -> a -> By8Bits a -> (Maybe a, By8Bits a)
insert key value (By8Bits a b c d) =
  if key < 128
    then if key < 64
      then
        By6Bits.insert key value a & second (\a -> By8Bits a b c d)
      else
        By6Bits.insert key value b & second (\b -> By8Bits a b c d)
    else if key < 192
      then
        By6Bits.insert key value c & second (\c -> By8Bits a b c d)
      else
        By6Bits.insert key value d & second (\d -> By8Bits a b c d)

foldrWithKey :: (Int -> a -> b -> b) -> b -> By8Bits a -> b
foldrWithKey step end (By8Bits part1 part2 part3 part4) =
  By6Bits.foldrWithKey step
    (By6Bits.foldrWithKey (offsetStep 64)
      (By6Bits.foldrWithKey (offsetStep 128)
        (By6Bits.foldrWithKey (offsetStep 192)
          end part4)
        part3)
      part2)
    part1
  where
    offsetStep offset key =
      step (key + offset)

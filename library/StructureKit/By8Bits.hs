module StructureKit.By8Bits
where

import StructureKit.Prelude
import PrimitiveExtras.By6Bits (By6Bits)
import qualified PrimitiveExtras.By6Bits as By6Bits


{-|
Map indexed with 8 bits.
-}
data By8Bits a =
  By8Bits (By6Bits a) (By6Bits a) (By6Bits a) (By6Bits a)

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
insert key =
  if key < 128
    then if key < 64
      then \value (By8Bits a b c d) ->
        error "TODO"
      else error "TODO"
    else if key < 192
      then error "TODO"
      else error "TODO"

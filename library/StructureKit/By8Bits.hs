module StructureKit.By8Bits
where

import StructureKit.Prelude
import PrimitiveExtras.By6Bits (By6Bits)


{-|
Map indexed with 8 bits.
-}
data By8Bits a =
  By8Bits
    (By6Bits a)
    (By6Bits a)
    (By6Bits a)
    (By6Bits a)

lookup :: Int -> By8Bits a -> Maybe a
lookup =
  error "TODO"

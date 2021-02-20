{-|
Map indexed by 18 bits.
-}
module StructureKit.By18Bits
(
  By18Bits,
  init,
  lookup,
  adjust,
)
where

import StructureKit.Prelude hiding (init, lookup, adjust)
import PrimitiveExtras.By6Bits (By6Bits)
import qualified PrimitiveExtras.By6Bits as By6Bits
import qualified StructureKit.TrieBitMasks as TrieBitMasks


newtype By18Bits a =
  By18Bits (By6Bits (By6Bits (By6Bits a)))

init :: By18Bits a
init =
  By18Bits By6Bits.empty

{-|
Lookup only using the first 18 bits.
-}
lookup :: Int -> By18Bits a -> Maybe a
lookup key (By18Bits tree1) =
  By6Bits.lookup (TrieBitMasks.level1 key) tree1 >>=
  By6Bits.lookup (TrieBitMasks.level2 key) >>=
  By6Bits.lookup (TrieBitMasks.level3 key)

adjust :: (a -> a) -> Int -> By18Bits a -> By18Bits a
adjust cont key =
  mapCoercible
    (flip By6Bits.adjust (TrieBitMasks.level1 key)
      (flip By6Bits.adjust (TrieBitMasks.level2 key)
        (flip By6Bits.adjust (TrieBitMasks.level3 key)
          cont)))

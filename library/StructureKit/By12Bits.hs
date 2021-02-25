{-|
Map indexed by 12 bits.
-}
module StructureKit.By12Bits
(
  By12Bits,
  empty,
  lookup,
  adjust,
  revision,
)
where

import StructureKit.Prelude hiding (empty, lookup, adjust)
import PrimitiveExtras.By6Bits (By6Bits)
import qualified PrimitiveExtras.By6Bits as By6Bits
import qualified StructureKit.TrieBitMasks as TrieBitMasks


newtype By12Bits a =
  By12Bits (By6Bits (By6Bits a))

empty :: By12Bits a
empty =
  By12Bits By6Bits.empty

{-|
Lookup only using the first 12 bits.
-}
lookup :: Int -> By12Bits a -> Maybe a
lookup key (By12Bits tree1) =
  By6Bits.lookup (TrieBitMasks.level1 key) tree1 >>=
  By6Bits.lookup (TrieBitMasks.level2 key)

adjust :: (a -> a) -> Int -> By12Bits a -> By12Bits a
adjust cont key =
  mapCoercible
    (flip By6Bits.adjust (TrieBitMasks.level1 key)
      (flip By6Bits.adjust (TrieBitMasks.level2 key)
        cont))

revision :: Functor f => Int -> f (Maybe a) -> (a -> f (Maybe a)) -> By12Bits a -> f (Maybe (By12Bits a))
revision key onAbsent onPresent (By12Bits trie) =
  By6Bits.revision
    (onAbsent & fmap (fmap (singletonTreeAtLevel2 key)))
    (By6Bits.revision
      onAbsent
      onPresent
      (TrieBitMasks.level2 key))
    (TrieBitMasks.level1 key)
    trie
    & fmap coerce

singletonTreeAtLevel1 key =
  By6Bits.singleton (TrieBitMasks.level1 key) .
  By6Bits.singleton (TrieBitMasks.level2 key)

singletonTreeAtLevel2 key =
  By6Bits.singleton (TrieBitMasks.level2 key)

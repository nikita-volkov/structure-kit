{-|
Map indexed by 18 bits.
-}
module StructureKit.By18Bits
(
  By18Bits,
  empty,
  lookup,
  adjust,
  revision,
)
where

import StructureKit.Prelude hiding (empty, lookup, adjust)
import StructureKit.By6Bits (By6Bits)
import qualified StructureKit.By6Bits as By6Bits
import qualified StructureKit.TrieBitMasks as TrieBitMasks


newtype By18Bits a =
  By18Bits (By6Bits (By6Bits (By6Bits a)))

empty :: By18Bits a
empty =
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

revision :: Functor f => Int -> f (Maybe a) -> (a -> f (Maybe a)) -> By18Bits a -> f (Maybe (By18Bits a))
revision key onAbsent onPresent (By18Bits trie) =
  By6Bits.revision
    (TrieBitMasks.level1 key)
    (onAbsent & fmap (fmap (singletonTreeAtLevel2 key)))
    (By6Bits.revision
      (TrieBitMasks.level2 key)
      (onAbsent & fmap (fmap (singletonTreeAtLevel3 key)))
      (By6Bits.revision
        (TrieBitMasks.level3 key)
        onAbsent
        onPresent))
    trie
    & fmap coerce

singletonTreeAtLevel1 key =
  By6Bits.singleton (TrieBitMasks.level1 key) .
  By6Bits.singleton (TrieBitMasks.level2 key) .
  By6Bits.singleton (TrieBitMasks.level3 key)

singletonTreeAtLevel2 key =
  By6Bits.singleton (TrieBitMasks.level2 key) .
  By6Bits.singleton (TrieBitMasks.level3 key)

singletonTreeAtLevel3 key =
  By6Bits.singleton (TrieBitMasks.level3 key)

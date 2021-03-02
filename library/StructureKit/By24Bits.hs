{-|
Map indexed by 24 bits.
-}
module StructureKit.By24Bits
(
  By24Bits,
  empty,
  lookup,
  adjust,
  mapAt,
  revision,
  dive,
)
where

import StructureKit.Prelude hiding (empty, lookup, adjust)
import StructureKit.By6Bits (By6Bits)
import qualified StructureKit.By6Bits as By6Bits
import qualified StructureKit.TrieBitMasks as TrieBitMasks


newtype By24Bits a =
  By24Bits (By6Bits (By6Bits (By6Bits (By6Bits a))))

empty :: By24Bits a
empty =
  By24Bits By6Bits.empty

{-|
Lookup only using the first 24 bits.
-}
lookup :: Int -> By24Bits a -> Maybe a
lookup key (By24Bits tree) =
  By6Bits.lookup (TrieBitMasks.level1 key) tree >>=
  By6Bits.lookup (TrieBitMasks.level2 key) >>=
  By6Bits.lookup (TrieBitMasks.level3 key) >>=
  By6Bits.lookup (TrieBitMasks.level4 key)

adjust :: (a -> a) -> Int -> By24Bits a -> By24Bits a
adjust cont key =
  mapCoercible
    (flip By6Bits.adjust (TrieBitMasks.level1 key)
      (flip By6Bits.adjust (TrieBitMasks.level2 key)
        (flip By6Bits.adjust (TrieBitMasks.level3 key)
          (flip By6Bits.adjust (TrieBitMasks.level4 key)
            cont))))

mapAt :: Int -> (a -> a) -> By24Bits a -> By24Bits a
mapAt key cont =
  adjust cont key

revision :: Functor f => Int -> f (Maybe a) -> (a -> f (Maybe a)) -> By24Bits a -> f (Maybe (By24Bits a))
revision key onAbsent onPresent (By24Bits trie) =
  By6Bits.revision
    (TrieBitMasks.level1 key)
    (onAbsent & fmap (fmap (singletonTreeAtLevel2 key)))
    (By6Bits.revision
      (TrieBitMasks.level2 key)
      (onAbsent & fmap (fmap (singletonTreeAtLevel3 key)))
      (By6Bits.revision
        (TrieBitMasks.level3 key)
        (onAbsent & fmap (fmap (singletonTreeAtLevel4 key)))
        (By6Bits.revision
          (TrieBitMasks.level4 key)
          onAbsent
          onPresent)))
    trie
    & fmap coerce

dive :: Int -> Maybe a -> (a -> Maybe a) -> By24Bits a -> (Maybe a, By24Bits a)
dive key onAbsent onPresent model =
  revision key (Nothing, onAbsent) (\a -> (Just a, onPresent a)) model
    & second (fromMaybe empty)

singletonTreeAtLevel1 key =
  By6Bits.singleton (TrieBitMasks.level1 key) .
  By6Bits.singleton (TrieBitMasks.level2 key) .
  By6Bits.singleton (TrieBitMasks.level3 key) .
  By6Bits.singleton (TrieBitMasks.level4 key)

singletonTreeAtLevel2 key =
  By6Bits.singleton (TrieBitMasks.level2 key) .
  By6Bits.singleton (TrieBitMasks.level3 key) .
  By6Bits.singleton (TrieBitMasks.level4 key)

singletonTreeAtLevel3 key =
  By6Bits.singleton (TrieBitMasks.level3 key) .
  By6Bits.singleton (TrieBitMasks.level4 key)

singletonTreeAtLevel4 key =
  By6Bits.singleton (TrieBitMasks.level4 key)

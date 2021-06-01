{-|
Map indexed by 30 bits.
-}
module StructureKit.By30Bits
(
  By30Bits,
  empty,
  lookup,
  adjust,
  mapAt,
  revise,
  dive,
)
where

import StructureKit.Prelude hiding (empty, lookup, adjust)
import StructureKit.By6Bits (By6Bits)
import qualified StructureKit.By6Bits as By6Bits
import qualified StructureKit.TrieBitMasks as TrieBitMasks


newtype By30Bits a =
  By30Bits (By6Bits (By6Bits (By6Bits (By6Bits (By6Bits a)))))

empty :: By30Bits a
empty =
  By30Bits By6Bits.empty

{-|
Lookup only using the first 30 bits.
-}
lookup :: Int -> By30Bits a -> Maybe a
lookup key (By30Bits tree) =
  By6Bits.lookup (TrieBitMasks.level1 key) tree >>=
  By6Bits.lookup (TrieBitMasks.level2 key) >>=
  By6Bits.lookup (TrieBitMasks.level3 key) >>=
  By6Bits.lookup (TrieBitMasks.level4 key) >>=
  By6Bits.lookup (TrieBitMasks.level5 key)

adjust :: (a -> a) -> Int -> By30Bits a -> By30Bits a
adjust cont key =
  mapCoercible
    (flip By6Bits.adjust (TrieBitMasks.level1 key)
      (flip By6Bits.adjust (TrieBitMasks.level2 key)
        (flip By6Bits.adjust (TrieBitMasks.level3 key)
          (flip By6Bits.adjust (TrieBitMasks.level4 key)
            (flip By6Bits.adjust (TrieBitMasks.level5 key)
              cont)))))

mapAt :: Int -> (a -> a) -> By30Bits a -> By30Bits a
mapAt key cont =
  adjust cont key

revise :: Functor f => Int -> f (Maybe a) -> (a -> f (Maybe a)) -> By30Bits a -> f (Maybe (By30Bits a))
revise key onAbsent onPresent (By30Bits trie) =
  By6Bits.revise
    (TrieBitMasks.level1 key)
    (onAbsent & fmap (fmap (singletonTreeAtLevel2 key)))
    (By6Bits.revise
      (TrieBitMasks.level2 key)
      (onAbsent & fmap (fmap (singletonTreeAtLevel3 key)))
      (By6Bits.revise
        (TrieBitMasks.level3 key)
        (onAbsent & fmap (fmap (singletonTreeAtLevel4 key)))
        (By6Bits.revise
          (TrieBitMasks.level4 key)
          (onAbsent & fmap (fmap (singletonTreeAtLevel5 key)))
          (By6Bits.revise
            (TrieBitMasks.level5 key)
            onAbsent
            onPresent))))
    trie
    & fmap coerce

dive :: Int -> Maybe a -> (a -> Maybe a) -> By30Bits a -> (Maybe a, By30Bits a)
dive key onAbsent onPresent model =
  revise key (Nothing, onAbsent) (\a -> (Just a, onPresent a)) model
    & second (fromMaybe empty)

singletonTreeAtLevel1 key =
  By6Bits.singleton (TrieBitMasks.level1 key) .
  By6Bits.singleton (TrieBitMasks.level2 key) .
  By6Bits.singleton (TrieBitMasks.level3 key) .
  By6Bits.singleton (TrieBitMasks.level4 key) .
  By6Bits.singleton (TrieBitMasks.level5 key)

singletonTreeAtLevel2 key =
  By6Bits.singleton (TrieBitMasks.level2 key) .
  By6Bits.singleton (TrieBitMasks.level3 key) .
  By6Bits.singleton (TrieBitMasks.level4 key) .
  By6Bits.singleton (TrieBitMasks.level5 key)

singletonTreeAtLevel3 key =
  By6Bits.singleton (TrieBitMasks.level3 key) .
  By6Bits.singleton (TrieBitMasks.level4 key) .
  By6Bits.singleton (TrieBitMasks.level5 key)

singletonTreeAtLevel4 key =
  By6Bits.singleton (TrieBitMasks.level4 key) .
  By6Bits.singleton (TrieBitMasks.level5 key)

singletonTreeAtLevel5 key =
  By6Bits.singleton (TrieBitMasks.level5 key)

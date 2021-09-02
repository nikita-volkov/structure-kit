-- |
-- Map indexed by 12 bits.
module StructureKit.By12Bits
  ( By12Bits,
    empty,
    lookup,
    adjust,
    revise,
    foldrWithKey,
  )
where

import StructureKit.By6Bits (By6Bits)
import qualified StructureKit.By6Bits as By6Bits
import StructureKit.Prelude hiding (adjust, empty, lookup)
import qualified StructureKit.TrieBitMasks as TrieBitMasks

newtype By12Bits a
  = By12Bits (By6Bits (By6Bits a))

empty :: By12Bits a
empty =
  By12Bits By6Bits.empty

-- |
-- Lookup only using the first 12 bits.
lookup :: Int -> By12Bits a -> Maybe a
lookup key (By12Bits tree1) =
  By6Bits.lookup (TrieBitMasks.level1 key) tree1
    >>= By6Bits.lookup (TrieBitMasks.level2 key)

adjust :: (a -> a) -> Int -> By12Bits a -> By12Bits a
adjust cont key =
  mapCoercible
    ( flip
        By6Bits.adjust
        (TrieBitMasks.level1 key)
        ( flip
            By6Bits.adjust
            (TrieBitMasks.level2 key)
            cont
        )
    )

revise :: Functor f => Int -> f (Maybe a) -> (a -> f (Maybe a)) -> By12Bits a -> f (Maybe (By12Bits a))
revise key onAbsent onPresent (By12Bits trie) =
  By6Bits.revise
    (TrieBitMasks.level1 key)
    (onAbsent & fmap (fmap (singletonTreeAtLevel2 key)))
    ( By6Bits.revise
        (TrieBitMasks.level2 key)
        onAbsent
        onPresent
    )
    trie
    & fmap coerce

foldrWithKey :: (Int -> a -> b -> b) -> b -> By12Bits a -> b
foldrWithKey step end (By12Bits trie1) =
  By6Bits.foldrWithKey
    ( \k1 trie2 next ->
        By6Bits.foldrWithKey (\k2 -> step (k1 .|. unsafeShiftL k2 6)) next trie2
    )
    end
    trie1

singletonTreeAtLevel1 key =
  By6Bits.singleton (TrieBitMasks.level1 key)
    . By6Bits.singleton (TrieBitMasks.level2 key)

singletonTreeAtLevel2 key =
  By6Bits.singleton (TrieBitMasks.level2 key)

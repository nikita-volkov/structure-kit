module StructureKit.Hamt
(
  Hamt,
  empty,
  findMapping,
  findAndReplace,
  revise,
  delete,
)
where

import StructureKit.Prelude hiding (empty, delete)
import Data.Primitive.SmallArray (SmallArray)
import qualified StructureKit.By30Bits as By30Bits
import qualified StructureKit.Util.SmallArray as SmallArray
import qualified Data.Primitive.SmallArray as SmallArray


newtype Hamt a =
  Hamt (By30Bits.By30Bits (SmallArray a))

empty :: Hamt a
empty =
  Hamt (By30Bits.empty)

findMapping :: Int -> (a -> Maybe b) -> Hamt a -> Maybe b
findMapping hash cont (Hamt trie) =
  By30Bits.lookup hash trie >>= SmallArray.findMapping cont

findAndReplace :: Int -> (a -> Maybe a) -> Hamt a -> (Maybe a, Hamt a)
findAndReplace hash cont (Hamt trie) =
  By30Bits.revise hash
    (Nothing, Nothing)
    (\array ->
      SmallArray.findAndReplace cont array
        & second Just)
    trie
    & second (Hamt . fromMaybe By30Bits.empty)

revise :: Functor f => Int -> (a -> Maybe b) -> f (Maybe a) -> (b -> f (Maybe a)) -> Hamt a -> f (Maybe (Hamt a))
revise hash select onMissing onPresent (Hamt trie) =
  By30Bits.revise hash
    (fmap (fmap pure) onMissing)
    (SmallArray.reviseSelected select onMissing onPresent)
    trie
    & fmap coerce

delete :: Int -> (a -> Maybe b) -> Hamt a -> (Maybe b, Maybe (Hamt a))
delete hash select (Hamt trie) =
  By30Bits.revise hash
    (Nothing, Nothing)
    (SmallArray.reviseSelected select
      (Nothing, Nothing)
      (\b -> (Just b, Nothing)))
    trie
    & fmap coerce

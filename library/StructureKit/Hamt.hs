module StructureKit.Hamt
(
  Hamt,
  empty,
  findMapping,
  findAndReplace,
  revision,
)
where

import StructureKit.Prelude hiding (empty)
import Data.Primitive.SmallArray (SmallArray)
import qualified StructureKit.By30Bits as By30Bits
import qualified PrimitiveExtras.SmallArray as SmallArray
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
  By30Bits.revisionAt hash
    (Nothing, Nothing)
    (\array ->
      SmallArray.findAndReplace cont array
        & second Just)
    trie
    & second (Hamt . fromMaybe By30Bits.empty)

revision :: Functor f => Int -> (a -> Maybe b) -> f (Maybe a) -> (b -> f (Maybe a)) -> Hamt a -> f (Maybe (Hamt a))
revision hash select onMissing onPresent (Hamt trie) =
  By30Bits.revisionAt hash
    (fmap (fmap pure) onMissing)
    (SmallArray.detectAndRevision select onMissing onPresent)
    trie
    & fmap coerce

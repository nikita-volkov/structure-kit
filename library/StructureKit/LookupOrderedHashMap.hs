module StructureKit.LookupOrderedHashMap
(
  LookupOrderedHashMap,
  lookup,
)
where

import StructureKit.Prelude hiding (lookup)
import qualified StructureKit.Hamt as Hamt
import qualified Deque.Strict as Deque


data LookupOrderedHashMap k v =
  LookupOrderedHashMap
    (Deque k)
    (Hamt.Hamt (Entry k v))

data Entry k v =
  PresentEntry
    {-| Amount of instances in deque. -}
    Int
    k v
  |
  MissingEntry Int k

lookup :: (Hashable k, Eq k) => k -> LookupOrderedHashMap k v -> (Maybe v, LookupOrderedHashMap k v)
lookup key (LookupOrderedHashMap deque trie) =
  Hamt.revision (hash key) select miss update trie
    & second (fromMaybe Hamt.empty)
    & second (LookupOrderedHashMap (Deque.snoc key deque))
  where
    select =
      \case
        PresentEntry count entryKey value ->
          if key == entryKey
            then Just (PresentEntry count entryKey value)
            else Nothing
        MissingEntry count entryKey ->
          if key == entryKey
            then Just (MissingEntry count entryKey)
            else Nothing
    miss =
      (Nothing, Just (MissingEntry 1 key))
    update =
      \case
        PresentEntry count entryKey value ->
          (Just value, Just (PresentEntry (succ count) entryKey value))
        MissingEntry count entryKey ->
          (Nothing, Just (MissingEntry (succ count) entryKey))


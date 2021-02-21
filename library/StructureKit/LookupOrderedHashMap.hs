module StructureKit.LookupOrderedHashMap
(
  LookupOrderedHashMap,
  lookup,
  evict,
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
    Int {-^ Amount of instances in deque. -}
    k v
  |
  MissingEntry Int k

lookup :: (Hashable k, Eq k) => k -> LookupOrderedHashMap k v -> (Maybe v, LookupOrderedHashMap k v)
lookup key (LookupOrderedHashMap deque trie) =
  Hamt.revision (hash key) (selectEntry key) miss update trie
    & second (fromMaybe Hamt.empty)
    & second (LookupOrderedHashMap (Deque.snoc key deque))
  where
    miss =
      (Nothing, Just (MissingEntry 1 key))
    update =
      \case
        PresentEntry count entryKey value ->
          (Just value, Just (PresentEntry (succ count) entryKey value))
        MissingEntry count entryKey ->
          (Nothing, Just (MissingEntry (succ count) entryKey))

{-|
Evict one entry from the map.
-}
evict :: (Hashable k, Eq k) => LookupOrderedHashMap k v -> (Maybe (k, v), LookupOrderedHashMap k v)
evict (LookupOrderedHashMap deque trie) =
  iterate deque trie
  where
    iterate deque trie =
      case Deque.uncons deque of
        Just (key, nextDeque) ->
          Hamt.revision (hash key) (selectEntry key) miss update trie
            & \(cont, hamt) -> cont (fromMaybe Hamt.empty hamt)
          where
            miss =
              (iterate nextDeque, Nothing)
            update =
              \case
                PresentEntry count entryKey value ->
                  if count == 1
                    then
                      (\trie -> (Just (entryKey, value), LookupOrderedHashMap nextDeque trie), Nothing)
                    else
                      (iterate nextDeque, Just (PresentEntry (pred count) entryKey value))
                MissingEntry count entryKey ->
                  if count == 1
                    then
                      (iterate nextDeque, Nothing)
                    else
                      (iterate nextDeque, Just (MissingEntry (pred count) entryKey))
        Nothing ->
          (Nothing, LookupOrderedHashMap deque trie)

entryKey :: Entry k v -> k
entryKey =
  \case
    PresentEntry count entryKey value -> entryKey
    MissingEntry count entryKey -> entryKey

selectEntry :: Eq k => k -> Entry k v -> Maybe (Entry k v)
selectEntry key entry =
  if entryKey entry == key
    then Just entry
    else Nothing

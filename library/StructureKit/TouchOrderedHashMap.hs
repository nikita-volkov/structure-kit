module StructureKit.TouchOrderedHashMap
(
  TouchOrderedHashMap,
  lookup,
  evict,
)
where

import StructureKit.Prelude hiding (lookup)
import qualified StructureKit.Hamt as Hamt
import qualified Deque.Strict as Deque


data TouchOrderedHashMap k v =
  TouchOrderedHashMap
    (Deque k)
    (Hamt.Hamt (Entry k v))

data Entry k v =
  Entry
    Int {-^ Amount of instances in deque. -}
    k v

lookup :: (Hashable k, Eq k) => k -> TouchOrderedHashMap k v -> (Maybe v, TouchOrderedHashMap k v)
lookup key (TouchOrderedHashMap deque trie) =
  revisionHamt key miss update trie
    & \(cont, trieMaybe) -> cont (fromMaybe Hamt.empty trieMaybe)
  where
    miss =
      (
        \trie -> (Nothing, TouchOrderedHashMap deque trie)
        ,
        Nothing
        )
    update (Entry count entryKey value) =
      (
        \trie -> (Just value, TouchOrderedHashMap (Deque.snoc entryKey deque) trie)
        ,
        Just (Entry (succ count) entryKey value)
        )

{-|
Evict one entry from the map.
-}
evict :: (Hashable k, Eq k) => TouchOrderedHashMap k v -> (Maybe (k, v), TouchOrderedHashMap k v)
evict (TouchOrderedHashMap deque trie) =
  iterate deque trie
  where
    iterate deque trie =
      case Deque.uncons deque of
        Just (key, nextDeque) ->
          revisionHamt key miss update trie
            & \(cont, trieMaybe) -> cont (fromMaybe Hamt.empty trieMaybe)
          where
            miss =
              (iterate nextDeque, Nothing)
            update =
              \case
                Entry count entryKey value ->
                  if count == 1
                    then
                      (\trie -> (Just (entryKey, value), TouchOrderedHashMap nextDeque trie),
                        Nothing)
                    else
                      (iterate nextDeque,
                        Just (Entry (pred count) entryKey value))
        Nothing ->
          (Nothing, TouchOrderedHashMap deque trie)

entryKey :: Entry k v -> k
entryKey =
  \case
    Entry count entryKey value -> entryKey

selectEntry :: Eq k => k -> Entry k v -> Maybe (Entry k v)
selectEntry key entry =
  if entryKey entry == key
    then Just entry
    else Nothing

revisionHamt :: (Functor f, Hashable k, Eq k) =>
  k ->
  f (Maybe (Entry k v)) -> (Entry k v -> f (Maybe (Entry k v))) ->
  Hamt.Hamt (Entry k v) -> f (Maybe (Hamt.Hamt (Entry k v)))
revisionHamt key =
  Hamt.revision (hash key) (selectEntry key)

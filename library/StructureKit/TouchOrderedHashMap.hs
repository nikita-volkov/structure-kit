module StructureKit.TouchOrderedHashMap
(
  TouchOrderedHashMap,
  empty,
  lookup,
  insert,
  revision,
  evict,
)
where

import StructureKit.Prelude hiding (lookup, insert, empty)
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

empty :: TouchOrderedHashMap k v
empty =
  TouchOrderedHashMap mempty Hamt.empty

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

insert :: (Hashable k, Eq k) => k -> v -> TouchOrderedHashMap k v -> (Maybe v, TouchOrderedHashMap k v)
insert key value (TouchOrderedHashMap deque trie) =
  revisionHamtFinalizing key miss update trie
  where
    miss =
      (finalizer, decision)
      where
        finalizer trie =
          (Nothing, TouchOrderedHashMap (Deque.snoc key deque) trie)
        decision =
          Just (Entry 1 key value)
    update (Entry count _ entryValue) =
      (finalizer, decision)
      where
        finalizer trie =
          (Just entryValue, TouchOrderedHashMap (Deque.snoc key deque) trie)
        decision =
          Just (Entry (succ count) key value)

revision :: (Hashable k, Eq k, Functor f) => k -> f (Maybe v) -> (v -> f (Maybe v)) -> TouchOrderedHashMap k v -> f (Maybe (TouchOrderedHashMap k v))
revision key miss update (TouchOrderedHashMap deque hamt) =
  revisionHamt key 
    (Compose (fmap
      (\case
        Just value ->
          (Deque.snoc key, Just (Entry 1 key value))
        Nothing ->
          (id, Nothing))
      miss))
    (\(Entry count _ oldValue) -> Compose (fmap
      (\case
        Just value ->
          (Deque.snoc key, Just (Entry (succ count) key value))
        Nothing ->
          (
            error "TODO: Delete all occurences of key in deque"
            ,
            Nothing
            ))
      (update oldValue)))
    hamt
    & \(Compose f) ->
        fmap
          (\(dequeMapper, hamtMaybe) ->
            fmap
              (TouchOrderedHashMap (dequeMapper deque))
              hamtMaybe)
          f
  where
    updateEntry =
      error "TODO"

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

revisionHamtFinalizing :: (Hashable k, Eq k) =>
  k ->
  (Hamt.Hamt (Entry k v) -> a, Maybe (Entry k v)) ->
  (Entry k v -> (Hamt.Hamt (Entry k v) -> a, Maybe (Entry k v))) ->
  Hamt.Hamt (Entry k v) ->
  a
revisionHamtFinalizing key miss update trie =
  revisionHamt key miss update trie
    & \(cont, trieMaybe) -> cont (fromMaybe Hamt.empty trieMaybe)

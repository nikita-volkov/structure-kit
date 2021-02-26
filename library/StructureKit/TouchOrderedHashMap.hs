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
    {-| Count of records in deque. -}
    Int
    k
    {-| Value if the record is not deleted. -}
    (Maybe v)

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
    update (Entry presence entryKey valueMaybe) =
      case valueMaybe of
        Just value ->
          (
            \trie -> (Just value, TouchOrderedHashMap (Deque.snoc entryKey deque) trie)
            ,
            Just (Entry (succ presence) entryKey (Just value))
            )
        Nothing ->
          (
            \trie -> (Nothing, TouchOrderedHashMap deque trie)
            ,
            Just (Entry presence entryKey Nothing)
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
          Just (Entry 1 key (Just value))
    update (Entry presence _ oldValueMaybe) =
      (finalizer, decision)
      where
        finalizer trie =
          (oldValueMaybe, TouchOrderedHashMap (Deque.snoc key deque) trie)
        decision =
          Just (Entry (succ presence) key (Just value))

revision :: (Hashable k, Eq k, Functor f) => k -> f (Maybe v) -> (v -> f (Maybe v)) -> TouchOrderedHashMap k v -> f (Maybe (TouchOrderedHashMap k v))
revision key miss update (TouchOrderedHashMap deque hamt) =
  revisionHamt key 
    (Compose (fmap
      (\case
        Just value ->
          (Deque.snoc key, Just (Entry 1 key (Just value)))
        Nothing ->
          (id, Nothing))
      miss))
    (\(Entry presence _ oldValueMaybe) -> Compose (fmap
      (\case
        Just value ->
          (Deque.snoc key, Just (Entry (succ presence) key (Just value)))
        Nothing ->
          (id,
            if presence == 1
              then Nothing
              else Just (Entry (pred presence) key Nothing)))
      (maybe miss update oldValueMaybe)))
    hamt
    & \(Compose f) ->
        fmap
          (\(dequeMapper, hamtMaybe) ->
            fmap
              (TouchOrderedHashMap (dequeMapper deque))
              hamtMaybe)
          f

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
            update (Entry presence key valueMaybe) =
              if presence == 1
                then
                  case valueMaybe of
                    Just value ->
                      (\trie -> (Just (key, value), TouchOrderedHashMap nextDeque trie),
                        Nothing)
                    Nothing ->
                      (iterate nextDeque,
                        Nothing)
                else
                  (iterate nextDeque,
                    Just (Entry (pred presence) key valueMaybe))
        Nothing ->
          (Nothing, TouchOrderedHashMap deque trie)

entryKey :: Entry k v -> k
entryKey =
  \case
    Entry _ entryKey _ -> entryKey

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

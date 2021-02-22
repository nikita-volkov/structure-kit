module StructureKit.LookupOrderedHashMap
(
  LookupOrderedHashMap,
  empty,
  lookup,
  insert,
  evict,
)
where

import StructureKit.Prelude hiding (lookup, insert, empty)
import qualified StructureKit.Hamt as Hamt
import qualified Deque.Strict as Deque


data LookupOrderedHashMap k v =
  LookupOrderedHashMap
    (Deque k)
    (Hamt.Hamt (Entry k v))

data Entry k v =
  MissingEntry Int k
  |
  PresentEntry Int k v

empty :: LookupOrderedHashMap k v
empty =
  LookupOrderedHashMap mempty Hamt.empty

lookup :: (Hashable k, Eq k) => k -> LookupOrderedHashMap k v -> (Maybe v, LookupOrderedHashMap k v)
lookup key (LookupOrderedHashMap deque trie) =
  revisionHamt key miss update trie
    & \(cont, trieMaybe) -> cont (fromMaybe Hamt.empty trieMaybe)
  where
    miss =
      (
        \trie -> (Nothing, LookupOrderedHashMap (Deque.snoc key deque) trie)
        ,
        Just (MissingEntry 1 key)
        )
    update =
      \case
        PresentEntry count entryKey value ->
          (
            \trie -> (Just value, LookupOrderedHashMap (Deque.snoc entryKey deque) trie)
            ,
            Just (PresentEntry (succ count) entryKey value)
            )
        MissingEntry count entryKey ->
          (
            \trie -> (Nothing, LookupOrderedHashMap (Deque.snoc entryKey deque) trie)
            ,
            Just (MissingEntry (succ count) entryKey)
            )

insert :: (Hashable k, Eq k) => k -> v -> LookupOrderedHashMap k v -> (Bool, LookupOrderedHashMap k v)
insert key value (LookupOrderedHashMap deque trie) =
  revisionHamtFinalizing key miss update trie
  where
    miss =
      (finalizer, decision)
      where
        finalizer trie =
          (True, LookupOrderedHashMap deque trie)
        decision =
          Just (PresentEntry 0 key value)
    update =
      \case
        PresentEntry count _ _ ->
          (finalizer, decision)
          where
            finalizer trie =
              (False, LookupOrderedHashMap deque trie)
            decision =
              Just (PresentEntry (succ count) key value)
        MissingEntry count _ ->
          (finalizer, decision)
          where
            finalizer trie =
              (True, LookupOrderedHashMap deque trie)
            decision =
              Just (PresentEntry (succ count) key value)

{-|
Evict one entry from the map.
-}
evict :: (Hashable k, Eq k) => LookupOrderedHashMap k v -> (Maybe (Maybe (k, v)), LookupOrderedHashMap k v)
evict (LookupOrderedHashMap deque trie) =
  case Deque.uncons deque of
    Just (key, nextDeque) ->
      revisionHamt key miss update trie
        & \(cont, trieMaybe) -> cont (fromMaybe Hamt.empty trieMaybe)
      where
        miss =
          (\trie -> (Just Nothing, LookupOrderedHashMap nextDeque trie),
            Nothing)
        update =
          \case
            PresentEntry count entryKey value ->
              if count == 0
                then
                  (\trie -> (Just (Just (entryKey, value)), LookupOrderedHashMap nextDeque trie),
                    Nothing)
                else
                  (\trie -> (Just Nothing, LookupOrderedHashMap nextDeque trie),
                    Just (PresentEntry (pred count) entryKey value))
            MissingEntry count entryKey ->
              if count == 0
                then
                  (\trie -> (Just Nothing, LookupOrderedHashMap nextDeque trie),
                    Nothing)
                else
                  (\trie -> (Just Nothing, LookupOrderedHashMap nextDeque trie),
                    Just (MissingEntry (pred count) entryKey))
    Nothing ->
      (Nothing, LookupOrderedHashMap deque trie)

entryKey :: Entry k v -> k
entryKey =
  \case
    MissingEntry count entryKey -> entryKey
    PresentEntry count entryKey value -> entryKey

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

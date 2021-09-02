module StructureKit.LookupOrderedHashMap
  ( LookupOrderedHashMap,
    empty,
    lookup,
    insert,
    evict,
  )
where

import qualified Deque.Strict as Deque
import qualified StructureKit.Hamt as Hamt
import qualified StructureKit.LookupOrderedHashMapEntry as Entry
import StructureKit.Prelude hiding (empty, insert, lookup)

data LookupOrderedHashMap k v
  = LookupOrderedHashMap
      (Deque k)
      (Hamt.Hamt (Entry.LookupOrderedHashMapEntry k v))

empty :: LookupOrderedHashMap k v
empty =
  LookupOrderedHashMap mempty Hamt.empty

lookup :: (Hashable k, Eq k) => k -> LookupOrderedHashMap k v -> (Maybe v, LookupOrderedHashMap k v)
lookup key (LookupOrderedHashMap deque trie) =
  reviseHamt key miss update trie
    & \(cont, trieMaybe) -> cont (fromMaybe Hamt.empty trieMaybe)
  where
    miss =
      ( \trie -> (Nothing, LookupOrderedHashMap (Deque.snoc key deque) trie),
        Just (Entry.missing key)
      )
    update =
      Entry.inc >>> \(valuePresent, newEntry) ->
        ( \trie -> (valuePresent, LookupOrderedHashMap (Deque.snoc key deque) trie),
          Just newEntry
        )

insert :: (Hashable k, Eq k) => k -> v -> LookupOrderedHashMap k v -> (Maybe v, LookupOrderedHashMap k v)
insert key value (LookupOrderedHashMap deque trie) =
  reviseHamtFinalizing key miss update trie
  where
    miss =
      ( \trie -> (Nothing, LookupOrderedHashMap deque trie),
        Just (Entry.present key value)
      )
    update =
      Entry.insert value >>> \(valuePresent, newEntry) ->
        ( \trie -> (valuePresent, LookupOrderedHashMap deque trie),
          Just newEntry
        )

-- |
-- Evict one entry from the map.
evict :: (Hashable k, Eq k) => LookupOrderedHashMap k v -> (Maybe (Maybe (k, v)), LookupOrderedHashMap k v)
evict (LookupOrderedHashMap deque trie) =
  case Deque.uncons deque of
    Just (key, nextDeque) ->
      reviseHamt key miss update trie
        & \(cont, trieMaybe) -> cont (fromMaybe Hamt.empty trieMaybe)
      where
        miss =
          ( \trie -> (Just Nothing, LookupOrderedHashMap nextDeque trie),
            Nothing
          )
        update entry =
          Entry.dec entry & \(valueMaybe, entryMaybe) ->
            ( \trie -> (Just (fmap (key,) valueMaybe), LookupOrderedHashMap nextDeque trie),
              entryMaybe
            )
    Nothing ->
      (Nothing, LookupOrderedHashMap deque trie)

reviseHamt ::
  (Functor f, Hashable k, Eq k) =>
  k ->
  f (Maybe (Entry.LookupOrderedHashMapEntry k v)) ->
  (Entry.LookupOrderedHashMapEntry k v -> f (Maybe (Entry.LookupOrderedHashMapEntry k v))) ->
  Hamt.Hamt (Entry.LookupOrderedHashMapEntry k v) ->
  f (Maybe (Hamt.Hamt (Entry.LookupOrderedHashMapEntry k v)))
reviseHamt key =
  Hamt.revise (hash key) (Entry.select key)

reviseHamtFinalizing ::
  (Hashable k, Eq k) =>
  k ->
  (Hamt.Hamt (Entry.LookupOrderedHashMapEntry k v) -> a, Maybe (Entry.LookupOrderedHashMapEntry k v)) ->
  (Entry.LookupOrderedHashMapEntry k v -> (Hamt.Hamt (Entry.LookupOrderedHashMapEntry k v) -> a, Maybe (Entry.LookupOrderedHashMapEntry k v))) ->
  Hamt.Hamt (Entry.LookupOrderedHashMapEntry k v) ->
  a
reviseHamtFinalizing key miss update trie =
  reviseHamt key miss update trie
    & \(cont, trieMaybe) -> cont (fromMaybe Hamt.empty trieMaybe)

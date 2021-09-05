module StructureKit.TouchOrderedHashMap
  ( -- *
    TouchOrderedHashMap,
    empty,
    lookup,
    insert,
    revise,
    evict,

    -- * Selection API
    select,

    -- ** Present
    Present,
    read,
    remove,
    overwrite,

    -- ** Missing
    Missing,
    write,
  )
where

import qualified Deque.Strict as Deque
import qualified StructureKit.Hamt as Hamt
import StructureKit.Prelude hiding (empty, insert, lookup, read, write)

data TouchOrderedHashMap k v
  = TouchOrderedHashMap
      (Deque k)
      (Hamt.Hamt (Entry k v))

data Entry k v
  = Entry
      Int
      -- ^ Count of records in deque.
      k
      -- ^ Key.
      (Maybe v)
      -- ^ Value if the record is not deleted.

empty :: TouchOrderedHashMap k v
empty =
  TouchOrderedHashMap mempty Hamt.empty

lookup :: (Hashable k, Eq k) => k -> TouchOrderedHashMap k v -> (Maybe v, TouchOrderedHashMap k v)
lookup key (TouchOrderedHashMap deque trie) =
  reviseHamt key miss update trie
    & \(cont, trieMaybe) -> cont (fromMaybe Hamt.empty trieMaybe)
  where
    miss =
      ( \trie -> (Nothing, TouchOrderedHashMap deque trie),
        Nothing
      )
    update (Entry presence entryKey valueMaybe) =
      case valueMaybe of
        Just value ->
          ( \trie -> (Just value, TouchOrderedHashMap (Deque.snoc entryKey deque) trie),
            Just (Entry (succ presence) entryKey (Just value))
          )
        Nothing ->
          ( \trie -> (Nothing, TouchOrderedHashMap deque trie),
            Just (Entry presence entryKey Nothing)
          )

insert :: (Hashable k, Eq k) => k -> v -> TouchOrderedHashMap k v -> (Maybe v, TouchOrderedHashMap k v)
insert key value (TouchOrderedHashMap deque trie) =
  reviseHamtFinalizing key miss update trie
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

revise :: (Hashable k, Eq k, Functor f) => k -> f (Maybe v) -> (v -> f (Maybe v)) -> TouchOrderedHashMap k v -> f (Maybe (TouchOrderedHashMap k v))
revise key miss update (TouchOrderedHashMap deque hamt) =
  reviseHamt
    key
    ( Compose
        ( fmap
            ( \case
                Just value ->
                  (Deque.snoc key, Just (Entry 1 key (Just value)))
                Nothing ->
                  (id, Nothing)
            )
            miss
        )
    )
    ( \(Entry presence _ oldValueMaybe) ->
        Compose
          ( fmap
              ( \case
                  Just value ->
                    (Deque.snoc key, Just (Entry (succ presence) key (Just value)))
                  Nothing ->
                    ( id,
                      if presence == 1
                        then Nothing
                        else Just (Entry (pred presence) key Nothing)
                    )
              )
              (maybe miss update oldValueMaybe)
          )
    )
    hamt
    & \(Compose f) ->
      fmap
        ( \(dequeMapper, hamtMaybe) ->
            fmap
              (TouchOrderedHashMap (dequeMapper deque))
              hamtMaybe
        )
        f

-- |
-- Evict one entry from the map.
evict :: (Hashable k, Eq k) => TouchOrderedHashMap k v -> (Maybe (k, v), TouchOrderedHashMap k v)
evict (TouchOrderedHashMap deque trie) =
  iterate deque trie
  where
    iterate deque trie =
      case Deque.uncons deque of
        Just (key, nextDeque) ->
          reviseHamt key miss update trie
            & \(cont, trieMaybe) -> cont (fromMaybe Hamt.empty trieMaybe)
          where
            miss =
              (iterate nextDeque, Nothing)
            update (Entry presence key valueMaybe) =
              if presence == 1
                then case valueMaybe of
                  Just value ->
                    ( \trie -> (Just (key, value), TouchOrderedHashMap nextDeque trie),
                      Nothing
                    )
                  Nothing ->
                    ( iterate nextDeque,
                      Nothing
                    )
                else
                  ( iterate nextDeque,
                    Just (Entry (pred presence) key valueMaybe)
                  )
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

reviseHamt ::
  (Functor f, Hashable k, Eq k) =>
  k ->
  f (Maybe (Entry k v)) ->
  (Entry k v -> f (Maybe (Entry k v))) ->
  Hamt.Hamt (Entry k v) ->
  f (Maybe (Hamt.Hamt (Entry k v)))
reviseHamt key =
  Hamt.revise (hash key) (selectEntry key)

reviseHamtFinalizing ::
  (Hashable k, Eq k) =>
  k ->
  (Hamt.Hamt (Entry k v) -> a, Maybe (Entry k v)) ->
  (Entry k v -> (Hamt.Hamt (Entry k v) -> a, Maybe (Entry k v))) ->
  Hamt.Hamt (Entry k v) ->
  a
reviseHamtFinalizing key miss update trie =
  reviseHamt key miss update trie
    & \(cont, trieMaybe) -> cont (fromMaybe Hamt.empty trieMaybe)

-- * Selection API

select :: k -> TouchOrderedHashMap k v -> Either (Missing k v) (Present k v)
select key (TouchOrderedHashMap deque hamt) =
  error "TODO"

-- **

data Present k v
  = Present ~v ~(TouchOrderedHashMap k v) (v -> TouchOrderedHashMap k v)

read :: Present k v -> v
read (Present x _ _) = x

remove :: Present k v -> TouchOrderedHashMap k v
remove (Present _ x _) = x

overwrite :: v -> Present k v -> TouchOrderedHashMap k v
overwrite val (Present _ _ x) = x val

-- **

newtype Missing k v
  = Missing (v -> TouchOrderedHashMap k v)

write :: v -> Missing k v -> TouchOrderedHashMap k v
write val (Missing x) = x val

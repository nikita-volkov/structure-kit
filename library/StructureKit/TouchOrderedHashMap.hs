module StructureKit.TouchOrderedHashMap
  ( -- *
    TouchOrderedHashMap,
    empty,
    lookup,
    insert,
    revise,
    evict,

    -- * Location API
    locate,

    -- ** Existing
    Existing,
    touch,
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
import StructureKit.Prelude hiding (empty, insert, lookup, read, touch, write)

data TouchOrderedHashMap k v
  = TouchOrderedHashMap
      (Deque k)
      (Hamt.Hamt (Entry k v))

data Entry k v = Entry
  { -- | Count of records in deque.
    entryCount :: Int,
    -- | Key.
    entryKey :: k,
    -- | Value if the record is not deleted.
    entryValue :: Maybe v
  }

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

locateEntry :: Eq k => k -> Entry k v -> Maybe (Entry k v)
locateEntry key entry =
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
  Hamt.revise (hash key) (locateEntry key)

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

-- * Location API

locate :: (Hashable k, Eq k) => k -> TouchOrderedHashMap k v -> Either (Missing k v) (Existing k v)
locate key (TouchOrderedHashMap deque hamt) =
  case Hamt.locate (hash key) ((key ==) . entryKey) hamt of
    Right hamtExisting ->
      error "TODO"
    Left _ ->
      error "TODO"

-- **

data Existing k v
  = Existing

touch :: Existing k v -> TouchOrderedHashMap k v
touch =
  error "TODO"

read :: Existing k v -> (v, TouchOrderedHashMap k v)
read =
  error "TODO"

remove :: Existing k v -> TouchOrderedHashMap k v
remove =
  error "TODO"

overwrite :: v -> Existing k v -> TouchOrderedHashMap k v
overwrite val =
  error "TODO"

-- **

data Missing k v

write :: v -> Missing k v -> TouchOrderedHashMap k v
write val =
  error "TODO"

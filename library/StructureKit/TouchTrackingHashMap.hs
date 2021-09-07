module StructureKit.TouchTrackingHashMap
  ( -- *
    TouchTrackingHashMap,
    empty,
    lookup,
    insert,
    evict,

    -- * Location API
    locate,

    -- ** Existing
    Existing,
    read,
    touch,
    overwrite,

    -- ** Missing
    Missing,
    write,
  )
where

import qualified Deque.Strict as Deque
import qualified StructureKit.Hamt as Hamt
import StructureKit.Prelude hiding (empty, insert, lookup, read, touch, write)

data TouchTrackingHashMap k v
  = TouchTrackingHashMap
      {-# UNPACK #-} !(Deque k)
      -- ^ Queue of touches to keys.
      {-# UNPACK #-} !(Hamt.Hamt (Entry k v))
      -- ^ Specialised hash map of entries.

data Entry k v = Entry
  { -- | Count of records in deque.
    entryCount :: {-# UNPACK #-} !Int,
    -- | Key.
    entryKey :: !k,
    -- | Value.
    entryValue :: v
  }

empty :: TouchTrackingHashMap k v
empty =
  TouchTrackingHashMap mempty Hamt.empty

lookup :: (Hashable k, Eq k) => k -> TouchTrackingHashMap k v -> (Maybe v, TouchTrackingHashMap k v)
lookup k tthm =
  locate k tthm & either (const (Nothing, tthm)) (liftA2 (,) (Just . read) touch)

insert :: (Hashable k, Eq k) => k -> v -> TouchTrackingHashMap k v -> (Maybe v, TouchTrackingHashMap k v)
insert key value tthm =
  locate key tthm & either (write value >>> (Nothing,)) (liftA2 (,) (Just . read) (overwrite value))

-- |
-- Evict one entry from the map.
evict :: (Hashable k, Eq k) => TouchTrackingHashMap k v -> (Maybe (k, v), TouchTrackingHashMap k v)
evict (TouchTrackingHashMap touches entries) =
  error "TODO"

-- * Location API

{-# INLINE locate #-}
locate :: (Hashable k, Eq k) => k -> TouchTrackingHashMap k v -> Either (Missing k v) (Existing k v)
locate key (TouchTrackingHashMap touches entries) =
  case Hamt.locate (hash key) ((==) key . entryKey) entries of
    Right existingEntry ->
      let Entry count key val = Hamt.read existingEntry
       in Right $ Existing count key val touches existingEntry
    Left missingEntry ->
      Left $ Missing key touches missingEntry

-- **

data Existing k v
  = Existing
      {-# UNPACK #-} !Int
      !k
      v
      {-# UNPACK #-} !(Deque.Deque k)
      {-# UNPACK #-} !(Hamt.Existing (Entry k v))

{-# INLINE read #-}
read :: (Hashable k, Eq k) => Existing k v -> v
read (Existing _ _ val _ _) = val

{-# INLINE touch #-}
touch :: (Hashable k, Eq k) => Existing k v -> TouchTrackingHashMap k v
touch (Existing count key val touches existingEntry) =
  let newEntry = Entry (succ count) key val
      newEntries = Hamt.overwrite newEntry existingEntry
      newTouches = Deque.snoc key touches
   in compact newTouches newEntries

{-# INLINE overwrite #-}
overwrite :: (Hashable k, Eq k) => v -> Existing k v -> TouchTrackingHashMap k v
overwrite newValue (Existing count key _ touches existingEntry) =
  let newEntry = Entry (succ count) key newValue
      newEntries = Hamt.overwrite newEntry existingEntry
      newTouches = Deque.snoc key touches
      newTthm = compact newTouches newEntries
   in newTthm

-- **

data Missing k v
  = Missing
      !k
      {-# UNPACK #-} !(Deque.Deque k)
      {-# UNPACK #-} !(Hamt.Missing (Entry k v))

{-# INLINE write #-}
write :: v -> Missing k v -> TouchTrackingHashMap k v
write val (Missing key touches missingEntry) =
  let entry = Entry 1 key val
      newEntries = Hamt.write entry missingEntry
      newTouches = Deque.snoc key touches
   in TouchTrackingHashMap newTouches newEntries

-- * Helpers

-- |
-- Manage the queue by dropping the entries with multiple appearances in it from its end
-- and construct a map from those.
{-# INLINE compact #-}
compact :: (Hashable k, Eq k) => Deque k -> Hamt.Hamt (Entry k v) -> TouchTrackingHashMap k v
compact !touches !entries =
  case Deque.uncons touches of
    Just (key, newTouches) ->
      case Hamt.locate (hash key) ((==) key . entryKey) entries of
        Right existingEntry ->
          case Hamt.read existingEntry of
            Entry count _ val ->
              if count == 1
                then TouchTrackingHashMap touches entries
                else
                  let newEntry = Entry (pred count) key val
                      newEntries = Hamt.overwrite newEntry existingEntry
                   in compact newTouches newEntries
        Left missingEntry ->
          error "Oops"
    Nothing ->
      TouchTrackingHashMap touches entries

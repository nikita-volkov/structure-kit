module StructureKit.TouchTrackingHashMap
  ( -- *
    TouchTrackingHashMap,
    empty,
    lookup,
    insert,
    evict,
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
lookup key (TouchTrackingHashMap touches entries) =
  case Hamt.locate (hash key) ((==) key . entryKey) entries of
    Right existingEntry ->
      let Entry count _ val = Hamt.read existingEntry
          newEntry = Entry (succ count) key val
          newEntries = Hamt.overwrite newEntry existingEntry
          newTouches = Deque.snoc key touches
          newTthm = compact newTouches newEntries
       in (Just val, newTthm)
    Left missingEntry ->
      (Nothing, TouchTrackingHashMap touches entries)

insert :: (Hashable k, Eq k) => k -> v -> TouchTrackingHashMap k v -> (Maybe v, TouchTrackingHashMap k v)
insert key value (TouchTrackingHashMap touches entries) =
  case Hamt.locate (hash key) ((==) key . entryKey) entries of
    Right existingEntry ->
      let Entry count _ val = Hamt.read existingEntry
          newEntry = Entry (succ count) key val
       in error "TODO"
    Left missingEntry ->
      error "TODO"

-- |
-- Evict one entry from the map.
evict :: (Hashable k, Eq k) => TouchTrackingHashMap k v -> (Maybe (k, v), TouchTrackingHashMap k v)
evict (TouchTrackingHashMap touches entries) =
  error "TODO"

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

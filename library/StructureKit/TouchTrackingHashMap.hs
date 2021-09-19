module StructureKit.TouchTrackingHashMap
  ( -- *
    TouchTrackingHashMap,
    empty,

    -- * Basic operations
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

    -- * Folding
    foldr,
    toList,
  )
where

import qualified Data.HashMap.Strict as HashMap
import qualified Deque.Strict as Deque
import StructureKit.Prelude hiding (empty, foldr, insert, lookup, read, toList, touch, write)

data TouchTrackingHashMap k v
  = TouchTrackingHashMap
      {-# UNPACK #-} !(Deque k)
      -- ^ Queue of touches to keys.
      !(HashMap.HashMap k (Entry v))
      -- ^ Specialised hash map of entries.
  deriving (Functor)

instance (NFData k, NFData v) => NFData (TouchTrackingHashMap k v) where
  rnf (TouchTrackingHashMap touches entries) = touches `deepseq` entries `deepseq` ()

data Entry v = Entry
  { -- | Count of records in deque.
    entryCount :: {-# UNPACK #-} !Int,
    -- | Value.
    entryValue :: v
  }
  deriving (Functor)

instance (NFData v) => NFData (Entry v) where
  rnf (Entry count val) = count `seq` val `deepseq` ()

empty :: TouchTrackingHashMap k v
empty =
  TouchTrackingHashMap mempty HashMap.empty

-- * Basics

{-# INLINE lookup #-}
lookup :: (Hashable k, Eq k) => k -> TouchTrackingHashMap k v -> Maybe (v, TouchTrackingHashMap k v)
lookup k (TouchTrackingHashMap touches entries) =
  case HashMap.lookup k entries of
    Just (Entry count value) ->
      let !newEntry = Entry (succ count) value
          newEntries = HashMap.insert k newEntry entries
          newTouches = Deque.snoc k touches
          newTthm = recurseCompacting newTouches newEntries
       in Just (value, newTthm)
    Nothing -> Nothing

-- |
-- Insert value possibly replacing an old one, in which case the old one is returned.
{-# INLINE insert #-}
insert :: (Hashable k, Eq k) => k -> v -> TouchTrackingHashMap k v -> (Maybe v, TouchTrackingHashMap k v)
insert k v (TouchTrackingHashMap touches entries) =
  case HashMap.alterF replaceEmitting k entries of
    (replacedVal, newEntries) ->
      let newTouches = Deque.snoc k touches
          newTthm = case replacedVal of
            Just _ -> recurseCompacting newTouches newEntries
            Nothing -> TouchTrackingHashMap newTouches newEntries
       in (replacedVal, newTthm)
  where
    replaceEmitting = \case
      Just (Entry count oldVal) -> (Just oldVal, Just $! Entry (succ count) v)
      Nothing -> (Nothing, Just $! Entry 1 v)

-- |
-- Evict one entry from the map.
{-# INLINE evict #-}
evict :: (Hashable k, Eq k) => TouchTrackingHashMap k v -> (Maybe (k, v), TouchTrackingHashMap k v)
evict (TouchTrackingHashMap touches entries) =
  recurseEvicting touches entries

-- * Location API

{-# INLINE locate #-}
locate :: (Hashable k, Eq k) => k -> TouchTrackingHashMap k v -> Either (Missing k v) (Existing k v)
locate key (TouchTrackingHashMap touches entries) =
  {-# SCC "locate" #-}
  case HashMap.lookup key entries of
    Just (Entry cnt val) -> Right (Existing cnt val key touches entries)
    Nothing -> Left (Missing key touches entries)

-- **

data Existing k v
  = Existing
      {-# UNPACK #-} !Int
      v
      !k
      {-# UNPACK #-} !(Deque.Deque k)
      !(HashMap.HashMap k (Entry v))

{-# INLINE read #-}
read :: Existing k v -> v
read (Existing _ val _ _ _) = val

{-# INLINE touch #-}
touch :: (Hashable k, Eq k) => Existing k v -> TouchTrackingHashMap k v
touch (Existing count val key touches entries) =
  let !newEntry = Entry (succ count) val
      newEntries = HashMap.insert key newEntry entries
      newTouches = Deque.snoc key touches
   in recurseCompacting newTouches newEntries

{-# INLINE overwrite #-}
overwrite :: (Hashable k, Eq k) => v -> Existing k v -> TouchTrackingHashMap k v
overwrite newValue (Existing count _ key touches entries) =
  let !newEntry = Entry (succ count) newValue
      newEntries = HashMap.insert key newEntry entries
      newTouches = Deque.snoc key touches
   in recurseCompacting newTouches newEntries

-- **

data Missing k v
  = Missing
      !k
      {-# UNPACK #-} !(Deque.Deque k)
      !(HashMap.HashMap k (Entry v))

{-# INLINE write #-}
write :: (Hashable k, Eq k) => v -> Missing k v -> TouchTrackingHashMap k v
write val (Missing key touches entries) =
  let !entry = Entry 1 val
      newEntries = HashMap.insert key entry entries
      newTouches = Deque.snoc key touches
   in TouchTrackingHashMap newTouches newEntries

-- * Folding

-- |
-- Fold right in eviction order.
{-# INLINE foldr #-}
foldr :: (Hashable k, Eq k) => (k -> v -> s -> s) -> s -> TouchTrackingHashMap k v -> s
foldr step state (TouchTrackingHashMap touches entries) =
  recurseFoldring step state touches entries

-- |
-- Convert to list in eviction order.
{-# INLINE toList #-}
toList :: (Hashable k, Eq k) => TouchTrackingHashMap k v -> [(k, v)]
toList =
  foldr (\k v s -> (k, v) : s) []

-- * Helpers

recurseFoldring :: (Hashable k, Eq k) => (k -> v -> s -> s) -> s -> Deque k -> HashMap.HashMap k (Entry v) -> s
recurseFoldring step end touches entries =
  case Deque.uncons touches of
    Just (key, newTouches) ->
      case HashMap.lookup key entries of
        Just (Entry count value) ->
          if count == 1
            then
              let newEntries = HashMap.delete key entries
               in step key value (recurseFoldring step end newTouches newEntries)
            else
              let !newEntry = Entry (pred count) value
                  newEntries = HashMap.insert key newEntry entries
               in recurseFoldring step end newTouches newEntries
        Nothing ->
          error "Oops"
    Nothing -> end

-- |
-- Manage the queue by dropping the entries with multiple appearances in it from its end
-- and construct a map from those.
{-# SCC recurseCompacting #-}
{-# NOINLINE recurseCompacting #-}
recurseCompacting :: (Hashable k, Eq k) => Deque k -> HashMap.HashMap k (Entry v) -> TouchTrackingHashMap k v
recurseCompacting !touches !entries =
  case Deque.uncons touches of
    Just (key, newTouches) ->
      case HashMap.lookup key entries of
        Just (Entry count value) ->
          if count == 1
            then TouchTrackingHashMap touches entries
            else case Entry (pred count) value of
              newEntry -> case HashMap.insert key newEntry entries of
                newEntries -> recurseCompacting newTouches newEntries
        Nothing ->
          error "Oops"
    Nothing ->
      TouchTrackingHashMap touches entries

{-# SCC recurseEvicting #-}
{-# NOINLINE recurseEvicting #-}
recurseEvicting :: (Hashable k, Eq k) => Deque k -> HashMap.HashMap k (Entry v) -> (Maybe (k, v), TouchTrackingHashMap k v)
recurseEvicting !touches !entries =
  case Deque.uncons touches of
    Just (key, newTouches) ->
      case HashMap.lookup key entries of
        Just (Entry count value) ->
          if count == 1
            then case HashMap.delete key entries of
              newEntries -> case recurseCompacting newTouches newEntries of
                tthm -> (Just (key, value), tthm)
            else case Entry (pred count) value of
              newEntry -> case HashMap.insert key newEntry entries of
                newEntries -> recurseEvicting newTouches newEntries
        Nothing ->
          error "Oops"
    Nothing ->
      TouchTrackingHashMap touches entries & \tthm -> (Nothing, tthm)

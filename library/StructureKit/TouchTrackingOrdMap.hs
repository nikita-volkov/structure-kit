module StructureKit.TouchTrackingOrdMap
  ( -- *
    TouchTrackingOrdMap,
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

import qualified Data.Map.Strict as Map
import qualified Deque.Strict as Deque
import StructureKit.Prelude hiding (empty, foldr, insert, lookup, read, toList, touch, write)

data TouchTrackingOrdMap k v
  = TouchTrackingOrdMap
      {-# UNPACK #-} !(Deque k)
      -- ^ Queue of touches to keys.
      !(Map.Map k (Entry v))
      -- ^ Specialised map of entries.
  deriving (Functor)

instance (NFData k, NFData v) => NFData (TouchTrackingOrdMap k v) where
  rnf (TouchTrackingOrdMap touches entries) = touches `deepseq` entries `deepseq` ()

data Entry v = Entry
  { -- | Count of records in deque.
    entryCount :: !Int,
    -- | Value.
    entryValue :: v
  }
  deriving (Functor)

instance (NFData v) => NFData (Entry v) where
  rnf (Entry count val) = count `seq` val `deepseq` ()

-- *

empty :: TouchTrackingOrdMap k v
empty =
  TouchTrackingOrdMap mempty Map.empty

-- * Basics

{-# INLINE lookup #-}
lookup :: (Ord k) => k -> TouchTrackingOrdMap k v -> Maybe (v, TouchTrackingOrdMap k v)
lookup k (TouchTrackingOrdMap touches entries) =
  Map.alterF
    ( \case
        Just (Entry count value) ->
          let !newEntry = Entry (succ count) value
           in (Just value, Just newEntry)
        Nothing -> (Nothing, Nothing)
    )
    k
    entries
    & \(res, newEntries) ->
      case res of
        Just value ->
          let newTouches = Deque.snoc k touches
              newTtom = recurseCompacting newTouches newEntries
           in Just (value, newTtom)
        Nothing -> Nothing

-- |
-- Insert value possibly replacing an old one, in which case the old one is returned.
{-# INLINE insert #-}
insert :: (Ord k) => k -> v -> TouchTrackingOrdMap k v -> (Maybe v, TouchTrackingOrdMap k v)
insert k v (TouchTrackingOrdMap touches entries) =
  case Map.alterF replaceEmitting k entries of
    (replacedVal, newEntries) ->
      let newTouches = Deque.snoc k touches
          newTtom = case replacedVal of
            Just _ -> recurseCompacting newTouches newEntries
            Nothing -> TouchTrackingOrdMap newTouches newEntries
       in (replacedVal, newTtom)
  where
    replaceEmitting = \case
      Just (Entry count oldVal) -> (Just oldVal, Just $! Entry (succ count) v)
      Nothing -> (Nothing, Just $! Entry 1 v)

-- |
-- Evict one entry from the map.
{-# INLINE evict #-}
evict :: (Ord k) => TouchTrackingOrdMap k v -> (Maybe (k, v), TouchTrackingOrdMap k v)
evict (TouchTrackingOrdMap touches entries) =
  recurseEvicting touches entries

-- * Location API

{-# INLINE locate #-}
locate :: (Ord k) => k -> TouchTrackingOrdMap k v -> Either (Missing k v) (Existing k v)
locate key (TouchTrackingOrdMap touches entries) =
  {-# SCC "locate" #-}
  case Map.lookup key entries of
    Just (Entry cnt val) -> Right (Existing cnt val key touches entries)
    Nothing -> Left (Missing key touches entries)

-- **

data Existing k v
  = Existing
      {-# UNPACK #-} !Int
      v
      !k
      {-# UNPACK #-} !(Deque.Deque k)
      !(Map.Map k (Entry v))

{-# INLINE read #-}
read :: Existing k v -> v
read (Existing _ val _ _ _) = val

{-# INLINE touch #-}
touch :: (Ord k) => Existing k v -> TouchTrackingOrdMap k v
touch (Existing count val key touches entries) =
  let !newEntry = Entry (succ count) val
      newEntries = Map.insert key newEntry entries
      newTouches = Deque.snoc key touches
   in recurseCompacting newTouches newEntries

{-# INLINE overwrite #-}
overwrite :: (Ord k) => v -> Existing k v -> TouchTrackingOrdMap k v
overwrite newValue (Existing count _ key touches entries) =
  let !newEntry = Entry (succ count) newValue
      newEntries = Map.insert key newEntry entries
      newTouches = Deque.snoc key touches
   in recurseCompacting newTouches newEntries

-- **

data Missing k v
  = Missing
      !k
      {-# UNPACK #-} !(Deque.Deque k)
      !(Map.Map k (Entry v))

{-# INLINE write #-}
write :: (Ord k) => v -> Missing k v -> TouchTrackingOrdMap k v
write val (Missing key touches entries) =
  let !entry = Entry 1 val
      newEntries = Map.insert key entry entries
      newTouches = Deque.snoc key touches
   in TouchTrackingOrdMap newTouches newEntries

-- * Folding

-- |
-- Fold right in eviction order.
{-# INLINE foldr #-}
foldr :: (Ord k) => (k -> v -> s -> s) -> s -> TouchTrackingOrdMap k v -> s
foldr step state (TouchTrackingOrdMap touches entries) =
  recurseFoldring step state touches entries

-- |
-- Convert to list in eviction order.
{-# INLINE toList #-}
toList :: (Ord k) => TouchTrackingOrdMap k v -> [(k, v)]
toList =
  foldr (\k v s -> (k, v) : s) []

-- * Helpers

recurseFoldring :: (Ord k) => (k -> v -> s -> s) -> s -> Deque k -> Map.Map k (Entry v) -> s
recurseFoldring step end touches entries =
  case Deque.uncons touches of
    Just (key, newTouches) ->
      case Map.lookup key entries of
        Just (Entry count value) ->
          if count == 1
            then
              let newEntries = Map.delete key entries
               in step key value (recurseFoldring step end newTouches newEntries)
            else
              let !newEntry = Entry (pred count) value
                  newEntries = Map.insert key newEntry entries
               in recurseFoldring step end newTouches newEntries
        Nothing ->
          error "Oops"
    Nothing -> end

-- |
-- Manage the queue by dropping the entries with multiple appearances in it from its end
-- and construct a map from those.
{-# SCC recurseCompacting #-}
recurseCompacting :: (Ord k) => Deque k -> Map.Map k (Entry v) -> TouchTrackingOrdMap k v
recurseCompacting !touches !entries =
  case Deque.uncons touches of
    Just (key, newTouches) ->
      case Map.lookup key entries of
        Just (Entry count value) ->
          if count == 1
            then TouchTrackingOrdMap touches entries
            else case Entry (pred count) value of
              newEntry -> case Map.insert key newEntry entries of
                newEntries -> recurseCompacting newTouches newEntries
        Nothing ->
          error "Oops"
    Nothing ->
      TouchTrackingOrdMap touches entries

{-# SCC recurseEvicting #-}
recurseEvicting :: (Ord k) => Deque k -> Map.Map k (Entry v) -> (Maybe (k, v), TouchTrackingOrdMap k v)
recurseEvicting !touches !entries =
  case Deque.uncons touches of
    Just (key, newTouches) ->
      case Map.lookup key entries of
        Just (Entry count value) ->
          if count == 1
            then case Map.delete key entries of
              newEntries -> case recurseCompacting newTouches newEntries of
                tthm -> (Just (key, value), tthm)
            else case Entry (pred count) value of
              newEntry -> case Map.insert key newEntry entries of
                newEntries -> recurseEvicting newTouches newEntries
        Nothing ->
          error "Oops"
    Nothing ->
      TouchTrackingOrdMap touches entries & \tthm -> (Nothing, tthm)

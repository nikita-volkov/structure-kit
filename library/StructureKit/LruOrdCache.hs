module StructureKit.LruOrdCache
  ( -- * --
    LruOrdCache,
    empty,

    -- * Basic operations
    lookup,
    insert,
    insertMany,

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

import Data.List qualified as List
import StructureKit.Prelude hiding (empty, foldr, insert, lookup, read, toList, touch)
import StructureKit.TouchTrackingOrdMap qualified as TouchTrackingOrdMap

data LruOrdCache k v
  = LruOrdCache
      -- | Slots occupied.
      !Int
      -- | Slots available.
      !Int
      !(TouchTrackingOrdMap.TouchTrackingOrdMap k v)
  deriving (Functor)

instance (NFData k, NFData v) => NFData (LruOrdCache k v) where
  rnf (LruOrdCache occ cap map) = occ `seq` cap `seq` map `deepseq` ()

empty ::
  -- | Maximum amount of entries to store at one moment.
  -- After it\'s reached the oldest entry will
  -- be discarded on each insert.
  Int ->
  LruOrdCache k v
empty capacity =
  LruOrdCache 0 capacity TouchTrackingOrdMap.empty

-- * Basics

{-# INLINE lookup #-}
lookup :: (Ord k) => k -> LruOrdCache k v -> (Maybe v, LruOrdCache k v)
lookup k (LruOrdCache occ cap mem) =
  case TouchTrackingOrdMap.lookup k mem of
    (v, mem) -> (v, LruOrdCache occ cap mem)

{-# INLINE insert #-}
insert :: (Ord k) => k -> v -> LruOrdCache k v -> (Maybe (k, v), LruOrdCache k v)
insert k v (LruOrdCache occ cap mem) =
  case TouchTrackingOrdMap.insert k v mem of
    (replaced, mem) -> case replaced of
      Just _ -> (Nothing, LruOrdCache occ cap mem)
      Nothing ->
        if occ == cap
          then case TouchTrackingOrdMap.evict mem of
            (eviction, mem) -> (eviction, LruOrdCache cap cap mem)
          else case succ occ of
            occ -> (Nothing, LruOrdCache occ cap mem)

insertMany :: (Ord k) => [(k, v)] -> LruOrdCache k v -> ([(k, v)], LruOrdCache k v)
insertMany inserts (LruOrdCache occ cap mem) =
  eliminateInsertsGrowingOcc occ mem inserts
  where
    eliminateInsertsGrowingOcc !occ !mem = \case
      (k, v) : inserts ->
        case TouchTrackingOrdMap.insert k v mem of
          (replaced, mem) -> case replaced of
            Just _ -> eliminateInsertsGrowingOcc occ mem inserts
            Nothing ->
              if occ == cap
                then evict [] mem inserts
                else eliminateInsertsGrowingOcc (succ occ) mem inserts
      _ -> ([], LruOrdCache occ cap mem)
    evict evictions mem inserts =
      case TouchTrackingOrdMap.evict mem of
        (eviction, mem) ->
          case maybe id (:) eviction evictions of
            evictions -> eliminateInsertsCapped evictions mem inserts
    eliminateInsertsCapped !evictions !mem = \case
      (k, v) : inserts ->
        case TouchTrackingOrdMap.insert k v mem of
          (replaced, mem) -> case replaced of
            Just _ -> eliminateInsertsCapped evictions mem inserts
            Nothing -> evict evictions mem inserts
      _ ->
        (evictions, LruOrdCache cap cap mem)

-- * Location API

locate :: (Ord k) => k -> LruOrdCache k v -> Either (Missing k v) (Existing k v)
locate k (LruOrdCache occupied capacity tthm) =
  case TouchTrackingOrdMap.locate k tthm of
    Right loc -> Right (Existing occupied capacity loc)
    Left loc -> Left (Missing occupied capacity loc)

-- ** --

data Existing k v
  = Existing
      !Int
      !Int
      !(TouchTrackingOrdMap.Existing k v)

{-# INLINE read #-}
read :: Existing k v -> v
read (Existing _ _ loc) =
  TouchTrackingOrdMap.read loc

{-# INLINE touch #-}
touch :: (Ord k) => Existing k v -> LruOrdCache k v
touch (Existing occupied capacity loc) =
  LruOrdCache occupied capacity (TouchTrackingOrdMap.touch loc)

{-# INLINE overwrite #-}
overwrite :: (Ord k) => v -> Existing k v -> LruOrdCache k v
overwrite val (Existing occupied capacity loc) =
  LruOrdCache occupied capacity (TouchTrackingOrdMap.overwrite val loc)

-- ** --

data Missing k v
  = Missing
      !Int
      !Int
      !(TouchTrackingOrdMap.Missing k v)

-- |
-- Insert a new value.
--
-- Possibly returns an evicted value.
{-# INLINE write #-}
write :: (Ord k) => v -> Missing k v -> (Maybe (k, v), LruOrdCache k v)
write val (Missing occupied capacity loc) =
  let tthm = TouchTrackingOrdMap.write val loc
   in if occupied == capacity
        then case TouchTrackingOrdMap.evict tthm of
          (eviction, tthm) ->
            let lru = LruOrdCache occupied capacity tthm
             in (eviction, lru)
        else
          let lru = LruOrdCache (succ occupied) capacity tthm
           in (Nothing, lru)

-- * Folding

-- |
-- Fold right in eviction order.
{-# INLINE foldr #-}
foldr :: (Ord k) => (k -> v -> s -> s) -> s -> LruOrdCache k v -> s
foldr step state (LruOrdCache _ _ mem) =
  TouchTrackingOrdMap.foldr step state mem

-- |
-- Convert to list in eviction order.
{-# INLINE toList #-}
toList :: (Ord k) => LruOrdCache k v -> [(k, v)]
toList (LruOrdCache _ _ mem) =
  TouchTrackingOrdMap.toList mem

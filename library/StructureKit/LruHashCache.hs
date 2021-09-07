module StructureKit.LruHashCache
  ( -- *
    LruHashCache,
    empty,
    lookup,
    insert,

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

import StructureKit.Prelude hiding (empty, insert, lookup, read, touch)
import qualified StructureKit.TouchTrackingHashMap as TouchTrackingHashMap

data LruHashCache k v
  = LruHashCache
      {-# UNPACK #-} !Int
      -- ^ Slots occupied.
      {-# UNPACK #-} !Int
      -- ^ Slots available.
      {-# UNPACK #-} !(TouchTrackingHashMap.TouchTrackingHashMap k v)

empty ::
  -- | Maximum amount of entries to store at one moment.
  -- After it\'s reached the oldest entry will
  -- be discarded on each insert.
  Int ->
  LruHashCache k v
empty capacity =
  LruHashCache 0 capacity TouchTrackingHashMap.empty

lookup :: (Hashable k, Eq k) => k -> LruHashCache k v -> (Maybe v, LruHashCache k v)
lookup k lhc =
  locate k lhc
    & either
      (const (Nothing, lhc))
      ((,) <$> Just . read <*> touch)

insert :: (Hashable k, Eq k) => k -> v -> LruHashCache k v -> (Maybe (k, v), LruHashCache k v)
insert k v lhc =
  locate k lhc
    & either
      (write v)
      ((,) Nothing <$> overwrite v)

locate :: (Hashable k, Eq k) => k -> LruHashCache k v -> Either (Missing k v) (Existing k v)
locate k (LruHashCache occupied capacity tthm) =
  case TouchTrackingHashMap.locate k tthm of
    Right loc -> Right (Existing occupied capacity loc)
    _ -> error "TODO"

-- **

data Existing k v
  = Existing
      Int
      Int
      (TouchTrackingHashMap.Existing k v)

read :: Existing k v -> v
read (Existing _ _ loc) =
  TouchTrackingHashMap.read loc

touch :: (Hashable k, Eq k) => Existing k v -> LruHashCache k v
touch (Existing occupied capacity loc) =
  LruHashCache occupied capacity (TouchTrackingHashMap.touch loc)

overwrite :: (Hashable k, Eq k) => v -> Existing k v -> LruHashCache k v
overwrite val (Existing occupied capacity loc) =
  LruHashCache occupied capacity (TouchTrackingHashMap.overwrite val loc)

-- **

data Missing k v
  = Missing
      Int
      Int
      (TouchTrackingHashMap.Missing k v)

-- |
-- Insert a new value.
--
-- Possibly returns an evicted value.
write :: (Hashable k, Eq k) => v -> Missing k v -> (Maybe (k, v), LruHashCache k v)
write val (Missing occupied capacity loc) =
  let tthm = TouchTrackingHashMap.write val loc
   in if occupied == capacity
        then case TouchTrackingHashMap.evict tthm of
          (eviction, tthm) ->
            let lru = LruHashCache occupied capacity tthm
             in (eviction, lru)
        else
          let lru = LruHashCache (succ occupied) capacity tthm
           in (Nothing, lru)

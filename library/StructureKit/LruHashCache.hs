module StructureKit.LruHashCache
  ( -- *
    LruHashCache,
    empty,

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

    -- * Derivative operations
    lookup,
    insert,
    insertMany,

    -- * Folding
    foldr,
    toList,
  )
where

import qualified Data.List as List
import StructureKit.Prelude hiding (empty, foldr, insert, lookup, read, toList, touch)
import qualified StructureKit.TouchTrackingHashMap as TouchTrackingHashMap

data LruHashCache k v
  = LruHashCache
      {-# UNPACK #-} !Int
      -- ^ Slots occupied.
      {-# UNPACK #-} !Int
      -- ^ Slots available.
      {-# UNPACK #-} !(TouchTrackingHashMap.TouchTrackingHashMap k v)
  deriving (Functor)

instance (NFData k, NFData v) => NFData (LruHashCache k v) where
  rnf (LruHashCache occ cap map) = occ `seq` cap `seq` map `deepseq` ()

empty ::
  -- | Maximum amount of entries to store at one moment.
  -- After it\'s reached the oldest entry will
  -- be discarded on each insert.
  Int ->
  LruHashCache k v
empty capacity =
  LruHashCache 0 capacity TouchTrackingHashMap.empty

locate :: (Hashable k, Eq k) => k -> LruHashCache k v -> Either (Missing k v) (Existing k v)
locate k (LruHashCache occupied capacity tthm) =
  case TouchTrackingHashMap.locate k tthm of
    Right loc -> Right (Existing occupied capacity loc)
    Left loc -> Left (Missing occupied capacity loc)

-- **

data Existing k v
  = Existing
      !Int
      !Int
      !(TouchTrackingHashMap.Existing k v)

{-# INLINE read #-}
read :: Existing k v -> v
read (Existing _ _ loc) =
  TouchTrackingHashMap.read loc

{-# INLINE touch #-}
touch :: (Hashable k, Eq k) => Existing k v -> LruHashCache k v
touch (Existing occupied capacity loc) =
  LruHashCache occupied capacity (TouchTrackingHashMap.touch loc)

{-# INLINE overwrite #-}
overwrite :: (Hashable k, Eq k) => v -> Existing k v -> LruHashCache k v
overwrite val (Existing occupied capacity loc) =
  LruHashCache occupied capacity (TouchTrackingHashMap.overwrite val loc)

-- **

data Missing k v
  = Missing
      !Int
      !Int
      !(TouchTrackingHashMap.Missing k v)

-- |
-- Insert a new value.
--
-- Possibly returns an evicted value.
{-# INLINE write #-}
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

-- * Derivatives

{-# INLINE lookup #-}
lookup :: (Hashable k, Eq k) => k -> LruHashCache k v -> (Maybe v, LruHashCache k v)
lookup k lhc =
  locate k lhc
    & either
      (const (Nothing, lhc))
      ((,) <$> Just . read <*> touch)

{-# INLINE insert #-}
insert :: (Hashable k, Eq k) => k -> v -> LruHashCache k v -> (Maybe (k, v), LruHashCache k v)
insert k v lhc =
  locate k lhc
    & either
      (write v)
      ((,) Nothing <$> overwrite v)

insertMany :: (Hashable k, Eq k) => [(k, v)] -> LruHashCache k v -> ([(k, v)], LruHashCache k v)
insertMany =
  \inserts lhc -> List.foldr step end inserts lhc []
  where
    step (k, v) next !lhc !evictions =
      case locate k lhc of
        Left loc ->
          case write v loc of
            (eviction, lhc) ->
              let evictions' = case eviction of
                    Just eviction -> eviction : evictions
                    Nothing -> evictions
               in next lhc evictions'
        Right loc ->
          next (overwrite v loc) evictions
    end lhc evictions =
      (evictions, lhc)

-- * Folding

-- |
-- Fold right in eviction order.
{-# INLINE foldr #-}
foldr :: (Hashable k, Eq k) => (k -> v -> s -> s) -> s -> LruHashCache k v -> s
foldr step state (LruHashCache _ _ mem) =
  TouchTrackingHashMap.foldr step state mem

-- |
-- Convert to list in eviction order.
{-# INLINE toList #-}
toList :: (Hashable k, Eq k) => LruHashCache k v -> [(k, v)]
toList (LruHashCache _ _ mem) =
  TouchTrackingHashMap.toList mem

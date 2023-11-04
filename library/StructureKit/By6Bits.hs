module StructureKit.By6Bits
  ( -- * --
    By6Bits,
    empty,
    singleton,
    lookup,
    insert,
    foldrWithKey,
    toList,
    null,

    -- * Location API
    locate,

    -- ** Existing
    Existing,
    read,
    remove,
    overwrite,

    -- ** Missing
    Missing,
    write,
  )
where

import StructureKit.Prelude hiding (empty, insert, lookup, null, read, remove, singleton, toList, write)
import StructureKit.Prelude qualified as Prelude
import StructureKit.Util.SmallArray qualified as SmallArray

-- |
-- Map indexed with 6 bits.
data By6Bits a
  = By6Bits
      {-# UNPACK #-} !Word64
      {-# UNPACK #-} !(SmallArray a)

deriving instance Functor By6Bits

instance (Show a) => Show (By6Bits a) where
  show = show . toList

instance (NFData a) => NFData (By6Bits a) where
  rnf (By6Bits a b) = rwhnf (seq a (rnf b))

{-# INLINE empty #-}
empty :: By6Bits a
empty =
  By6Bits 0 Prelude.empty

-- |
-- An array with a single element at the specified index.
{-# INLINE singleton #-}
singleton :: Int -> a -> By6Bits a
singleton key a =
  let keys = bit key
      array = runST (newSmallArray 1 a >>= unsafeFreezeSmallArray)
   in By6Bits keys array

lookup :: Int -> By6Bits a -> Maybe a
lookup key =
  either (const Nothing) (Just . read) . locate key

insert :: Int -> a -> By6Bits a -> (Maybe a, By6Bits a)
insert key val map =
  map
    & locate key
    & either
      ((Nothing,) . write val)
      (liftA2 (,) (Just . read) (overwrite val))

{-# INLINE foldrWithKey #-}
foldrWithKey :: (Int -> a -> b -> b) -> b -> By6Bits a -> b
foldrWithKey step end (By6Bits bits array) =
  loop 0 0
  where
    loop key arrayIndex =
      if key < 64
        then
          if testBit bits key
            then case indexSmallArray array arrayIndex of
              element -> step key element (loop (succ key) (succ arrayIndex))
            else loop (succ key) arrayIndex
        else end

{-# INLINE toList #-}
toList :: By6Bits a -> [(Int, a)]
toList =
  foldrWithKey (\k v -> (:) (k, v)) []

{-# INLINE null #-}
null :: By6Bits a -> Bool
null (By6Bits keys _) = keys == 0

-- * Location API

{-# INLINE locate #-}
locate :: Int -> By6Bits a -> Either (Missing a) (Existing a)
locate key (By6Bits keys arr) =
  {-# SCC "locate" #-}
  let !keySingleton = bit key
      !idx = popCount (pred keySingleton .&. keys)
   in if keySingleton .&. keys == 0
        then Left $ Missing (keySingleton .|. keys) idx arr
        else Right $ Existing keySingleton keys idx arr

-- ** --

data Existing a
  = Existing
      -- | Singleton set pointing to the value.
      {-# UNPACK #-} !Word64
      -- | Old key set.
      {-# UNPACK #-} !Word64
      -- | Index in array.
      {-# UNPACK #-} !Int
      -- | Array.
      {-# UNPACK #-} !(SmallArray a)

{-# INLINE read #-}
read :: Existing a -> a
read (Existing _ _ idx arr) =
  indexSmallArray arr idx

{-# INLINE remove #-}
remove :: Existing a -> By6Bits a
remove (Existing keySingleton keys idx arr) =
  By6Bits (keys .&. complement keySingleton) (SmallArray.unset idx arr)

{-# INLINE overwrite #-}
overwrite :: a -> Existing a -> By6Bits a
overwrite val (Existing _ keys idx array) =
  By6Bits keys (SmallArray.set idx val array)

-- ** --

data Missing a
  = Missing
      -- | Key bitmap with this key.
      {-# UNPACK #-} !Word64
      -- | Found index in the array.
      {-# UNPACK #-} !Int
      -- | Array of entries.
      {-# UNPACK #-} !(SmallArray a)

{-# INLINE write #-}
write :: a -> Missing a -> By6Bits a
write val (Missing keysWithIt idx array) =
  By6Bits keysWithIt (SmallArray.insert idx val array)

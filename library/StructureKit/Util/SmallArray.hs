module StructureKit.Util.SmallArray
  ( newEmptySmallArray,
    singleton,
    list,
    unset,
    set,
    unsafeSetWithSize,
    insert,
    adjust,
    unsafeAdjust,
    unsafeAdjustWithSize,
    unsafeIndexAndAdjust,
    unsafeIndexAndAdjustWithSize,
    unsafeAdjustF,
    cons,
    snoc,
    orderedPair,
    reviseSelected,
    findAndReplace,
    findReplacing,
    findMapping,
    find,
    findWithIndex,
    drop,
    split,
    foldrInRange,
    inRangeUnfoldr,
    null,

    -- * Location API
    locateByPredicate,

    -- ** Existing
    Existing,
    read,
    remove,
    overwrite,
  )
where

import StructureKit.Prelude hiding (drop, find, insert, null, read, remove, singleton, snoc)

-- | A workaround for the weird forcing of 'undefined' values int 'newSmallArray'
{-# INLINE newEmptySmallArray #-}
newEmptySmallArray :: PrimMonad m => Int -> m (SmallMutableArray (PrimState m) a)
newEmptySmallArray size = newSmallArray size (unsafeCoerce 0)

singleton :: a -> SmallArray a
singleton value =
  runST (newSmallArray 1 value >>= unsafeFreezeSmallArray)

{-# INLINE list #-}
list :: [a] -> SmallArray a
list list =
  let !size = length list
   in runSmallArray $ do
        m <- newEmptySmallArray size
        let populate index list = case list of
              element : list -> do
                writeSmallArray m index element
                populate (succ index) list
              [] -> return m
         in populate 0 list

-- |
-- Remove an element.
{-# INLINE unset #-}
unset :: Int -> SmallArray a -> SmallArray a
unset index array =
  {-# SCC "unset" #-}
  let !size = sizeofSmallArray array
      !newSize = pred size
      !newIndex = succ index
      !amountOfFollowingElements = size - newIndex
   in runSmallArray $ do
        newMa <- newSmallArray newSize undefined
        copySmallArray newMa 0 array 0 index
        copySmallArray newMa index array newIndex amountOfFollowingElements
        return newMa

{-# INLINE set #-}
set :: Int -> a -> SmallArray a -> SmallArray a
set index a array =
  {-# SCC "set" #-}
  unsafeSetWithSize index a (sizeofSmallArray array) array

{-# INLINE unsafeSetWithSize #-}
unsafeSetWithSize :: Int -> a -> Int -> SmallArray a -> SmallArray a
unsafeSetWithSize index a size array =
  runST $ do
    m <- thawSmallArray array 0 size
    writeSmallArray m index a
    unsafeFreezeSmallArray m

{-# INLINE insert #-}
insert :: Int -> a -> SmallArray a -> SmallArray a
insert index a array =
  {-# SCC "insert" #-}
  let !size = sizeofSmallArray array
      !newSize = succ size
      !nextIndex = succ index
      !amountOfFollowingElements = size - index
   in runSmallArray $ do
        newMa <- newSmallArray newSize a
        copySmallArray newMa 0 array 0 index
        copySmallArray newMa nextIndex array index amountOfFollowingElements
        return newMa

{-# INLINE adjust #-}
adjust :: (a -> a) -> Int -> SmallArray a -> SmallArray a
adjust fn index array =
  let size = sizeofSmallArray array
   in if size > index && index >= 0
        then unsafeAdjustWithSize fn index size array
        else array

{-# INLINE unsafeAdjust #-}
unsafeAdjust :: (a -> a) -> Int -> SmallArray a -> SmallArray a
unsafeAdjust fn index array =
  unsafeAdjustWithSize fn index (sizeofSmallArray array) array

{-# INLINE unsafeAdjustWithSize #-}
unsafeAdjustWithSize :: (a -> a) -> Int -> Int -> SmallArray a -> SmallArray a
unsafeAdjustWithSize fn index size array =
  runST $ do
    m <- thawSmallArray array 0 size
    element <- readSmallArray m index
    writeSmallArray m index $! fn element
    unsafeFreezeSmallArray m

{-# INLINE unsafeIndexAndAdjust #-}
unsafeIndexAndAdjust :: (a -> a) -> Int -> SmallArray a -> (a, SmallArray a)
unsafeIndexAndAdjust fn index array =
  unsafeIndexAndAdjustWithSize fn index (sizeofSmallArray array) array

{-# INLINE unsafeIndexAndAdjustWithSize #-}
unsafeIndexAndAdjustWithSize :: (a -> a) -> Int -> Int -> SmallArray a -> (a, SmallArray a)
unsafeIndexAndAdjustWithSize fn index size array =
  runST $ do
    m <- thawSmallArray array 0 size
    element <- readSmallArray m index
    writeSmallArray m index $! fn element
    newArray <- unsafeFreezeSmallArray m
    return (element, newArray)

{-# INLINE unsafeAdjustF #-}
unsafeAdjustF :: Functor f => (a -> f a) -> Int -> SmallArray a -> f (SmallArray a)
unsafeAdjustF fn index array =
  fn (indexSmallArray array index)
    & fmap (\newElement -> set index newElement array)

{-# INLINE cons #-}
cons :: a -> SmallArray a -> SmallArray a
cons a array =
  {-# SCC "cons" #-}
  let size = sizeofSmallArray array
      newSize = succ size
   in runSmallArray $ do
        newMa <- newSmallArray newSize a
        copySmallArray newMa 1 array 0 size
        return newMa

{-# INLINE snoc #-}
snoc :: a -> SmallArray a -> SmallArray a
snoc a array =
  {-# SCC "snoc" #-}
  let size = sizeofSmallArray array
      newSize = succ size
   in runSmallArray $ do
        newMa <- newSmallArray newSize a
        copySmallArray newMa 0 array 0 size
        return newMa

{-# INLINE orderedPair #-}
orderedPair :: Int -> e -> Int -> e -> SmallArray e
orderedPair i1 e1 i2 e2 =
  {-# SCC "orderedPair" #-}
  runSmallArray $
    if
        | i1 < i2 -> do
          a <- newSmallArray 2 e1
          writeSmallArray a 1 e2
          return a
        | i1 > i2 -> do
          a <- newSmallArray 2 e1
          writeSmallArray a 0 e2
          return a
        | otherwise -> do
          a <- newSmallArray 1 e2
          return a

{-# INLINE reviseSelected #-}
reviseSelected :: Functor f => (a -> Maybe b) -> f (Maybe a) -> (b -> f (Maybe a)) -> SmallArray a -> f (Maybe (SmallArray a))
reviseSelected select onMissing onPresent array =
  let size = sizeofSmallArray array
      iterate index =
        if index < size
          then
            let element = indexSmallArray array index
             in case select element of
                  Just detectedElement ->
                    onPresent detectedElement
                      & fmap
                        ( \case
                            Just newElement -> Just (unsafeSetWithSize index newElement size array)
                            Nothing ->
                              if size == 1
                                then Nothing
                                else Just (unset index array)
                        )
                  Nothing ->
                    iterate (succ index)
          else
            onMissing
              & fmap
                ( \case
                    Just newElement -> Just (cons newElement array)
                    Nothing -> Just array
                )
   in iterate 0

-- |
-- Find the first matching element,
-- return it and new array which has it replaced by the value produced
-- by the supplied continuation.
{-# INLINE findAndReplace #-}
findAndReplace :: (a -> Maybe a) -> SmallArray a -> (Maybe a, SmallArray a)
findAndReplace f array =
  let size = sizeofSmallArray array
      iterate index =
        if index < size
          then
            let element = indexSmallArray array index
             in case f element of
                  Just newElement -> (Just element, set index newElement array)
                  Nothing -> iterate (succ index)
          else (Nothing, array)
   in iterate 0

-- |
-- Find the first matching element and replace it.
{-# INLINE findReplacing #-}
findReplacing :: (a -> Maybe a) -> SmallArray a -> SmallArray a
findReplacing f array =
  let size = sizeofSmallArray array
      iterate index =
        if index < size
          then case f (indexSmallArray array index) of
            Just newElement -> set index newElement array
            Nothing -> iterate (succ index)
          else array
   in iterate 0

-- |
-- Find the first matching element and return it.
{-# INLINE findMapping #-}
findMapping :: (a -> Maybe b) -> SmallArray a -> Maybe b
findMapping f array =
  let size = sizeofSmallArray array
      iterate index =
        if index < size
          then case f (indexSmallArray array index) of
            Just b -> Just b
            Nothing -> iterate (succ index)
          else Nothing
   in iterate 0

{-# INLINE find #-}
find :: (a -> Bool) -> SmallArray a -> Maybe a
find test array =
  {-# SCC "find" #-}
  let !size = sizeofSmallArray array
      iterate !index =
        if index < size
          then
            let !element = indexSmallArray array index
             in if test element
                  then Just element
                  else iterate (succ index)
          else Nothing
   in iterate 0

{-# INLINE findWithIndex #-}
findWithIndex :: (a -> Bool) -> SmallArray a -> Maybe (Int, a)
findWithIndex test =
  findWithIndexCont test Nothing (\i a -> Just (i, a))

{-# INLINE findWithIndexCont #-}
findWithIndexCont :: (a -> Bool) -> b -> (Int -> a -> b) -> SmallArray a -> b
findWithIndexCont test none some array =
  {-# SCC "findWithIndexCont" #-}
  let !size = sizeofSmallArray array
      iterate !index =
        if index < size
          then
            let element = indexSmallArray array index
             in if test element
                  then some index element
                  else iterate (succ index)
          else none
   in iterate 0

drop :: Int -> SmallArray a -> SmallArray a
drop amount array =
  cloneSmallArray array amount (sizeofSmallArray array - amount)

split :: Int -> SmallArray a -> (SmallArray a, SmallArray a)
split amount array =
  ( cloneSmallArray array 0 amount,
    cloneSmallArray array amount (sizeofSmallArray array - amount)
  )

foldrInRange :: Int -> Int -> (a -> b -> b) -> b -> SmallArray a -> b
foldrInRange start indexAfter step acc array =
  loop start
  where
    loop index =
      if index < indexAfter
        then case indexSmallArray## array index of (# a #) -> step a (loop (succ index))
        else acc

inRangeUnfoldr :: Int -> Int -> SmallArray a -> Unfoldr a
inRangeUnfoldr startIndex afterIndex array =
  Unfoldr (\step acc -> foldrInRange startIndex afterIndex step acc array)

null :: SmallArray a -> Bool
null = (== 0) . sizeofSmallArray

-- * Location API

locateByPredicate :: (a -> Bool) -> SmallArray a -> Maybe (Existing a)
locateByPredicate pred arr =
  findWithIndexCont pred Nothing (\i a -> Just (Existing a i arr)) arr

-- **

data Existing a
  = Existing
      a
      {-# UNPACK #-} !Int
      !(SmallArray a)

{-# INLINE read #-}
read :: Existing a -> a
read (Existing val _ _) = val

{-# INLINE remove #-}
remove :: Existing a -> SmallArray a
remove (Existing _ i arr) = unset i arr

{-# INLINE overwrite #-}
overwrite :: a -> Existing a -> SmallArray a
overwrite val (Existing _ i arr) = set i val arr

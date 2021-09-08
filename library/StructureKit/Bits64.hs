{-# OPTIONS_GHC -Wno-overflowed-literals #-}

-- |
-- Set of 6-bit values represented with a 64-bit word.
module StructureKit.Bits64
  ( Bits64,
    empty,
    null,
    size,
    singleton,
    member,
    lookup,
    insert,
    delete,
    revise,
    split,
    foldr,
    foldl',
    foldlM,
    forM_,
    unfoldr,
    toList,

    -- *
    locate,
    Location (..),
  )
where

import StructureKit.Prelude hiding (adjust, delete, empty, foldl', foldlM, foldr, forM_, insert, locate, lookup, member, null, read, remove, singleton, split, toList, unfoldr, write)

newtype Bits64
  = Bits64 Word64
  deriving (Eq, NFData)

instance Semigroup Bits64 where
  Bits64 l <> Bits64 r = Bits64 (l .|. r)

instance Monoid Bits64 where
  mempty = Bits64 0

instance Show Bits64 where
  show (Bits64 x) = showBinFinite x

empty :: Bits64
empty =
  Bits64 0

{-# INLINE null #-}
null :: Bits64 -> Bool
null (Bits64 word) =
  word == 0

size :: Bits64 -> Int
size (Bits64 word) =
  popCount word

singleton :: Int -> Bits64
singleton value =
  Bits64 (bit value)

member :: Int -> Bits64 -> Bool
member value (Bits64 word) =
  testBit word value

lookup :: Int -> Bits64 -> Maybe Int
lookup value (Bits64 word) =
  if value == 0
    then
      if word .&. 1 /= 0
        then Just 0
        else Nothing
    else
      let bitAtValue = bit value
          isPopulated = bitAtValue .&. word /= 0
       in if isPopulated
            then Just (popCount (word .&. (bitAtValue - 1)))
            else Nothing

insert :: Int -> Bits64 -> (Int, Bits64)
insert value (Bits64 word) =
  let bitAtValue = bit value
      index = popCount (word .&. (bitAtValue - 1))
      newWord = word .|. bitAtValue
   in (index, Bits64 newWord)

delete :: Int -> Bits64 -> (Int, Bits64)
delete value (Bits64 word) =
  let bitAtValue = bit value
      newWord = xor word bitAtValue
      index = popCount (word .&. pred bitAtValue)
   in (index, Bits64 newWord)

revise :: Functor f => Int -> (Int -> f Bool) -> (Int -> f Bool) -> Bits64 -> f (Maybe Bits64)
revise value onMissing onPresent (Bits64 word) =
  if value == 0
    then choose 1 0
    else
      let bitAtValue = bit value
       in choose bitAtValue (popCount (word .&. (bitAtValue - 1)))
  where
    choose bitAtValue populatedIndex =
      if word .&. bitAtValue /= 0
        then
          onPresent populatedIndex
            & fmap
              ( \case
                  True -> Just (Bits64 word)
                  False ->
                    let newWord = xor word bitAtValue
                     in if newWord == 0
                          then Nothing
                          else Just (Bits64 newWord)
              )
        else
          onMissing populatedIndex
            & fmap
              ( \case
                  True ->
                    let newWord = word .|. bitAtValue
                     in Just (Bits64 newWord)
                  False ->
                    if word == 0
                      then Nothing
                      else Just (Bits64 word)
              )

-- |
-- Splits the set in two.
-- The first one contains all elements smaller than the specified threshold,
-- the second - all remaining elements.
split :: Int -> Bits64 -> (Bits64, Bits64)
split value (Bits64 word) =
  if value == 0
    then (Bits64 0, Bits64 word)
    else
      let bitAtValue = bit value
          predMask = pred bitAtValue
          word1 = word .&. predMask
          word2 = xor word word1
       in (Bits64 word1, Bits64 word2)

foldr :: (Int -> a -> a) -> a -> Bits64 -> a
foldr step acc (Bits64 word) =
  loop 0
  where
    loop i =
      if i < 64
        then
          if testBit word i
            then step i (loop (succ i))
            else loop (succ i)
        else acc

foldl' :: (a -> Int -> a) -> a -> Bits64 -> a
foldl' step acc (Bits64 word) =
  loop 0 acc
  where
    loop i !acc =
      if i < 64
        then loop (succ i) (if testBit word i then step acc i else acc)
        else acc

foldlM :: Monad m => (a -> Int -> m a) -> a -> Bits64 -> m a
foldlM step acc (Bits64 word) =
  loop 0 acc
  where
    loop i !acc =
      if i < 64
        then
          if testBit word i
            then step acc i >>= loop (succ i)
            else loop (succ i) acc
        else return acc

forM_ :: Monad m => (Int -> m ()) -> Bits64 -> m ()
forM_ fn (Bits64 word) =
  loop 0
  where
    loop i =
      if i < 64
        then
          if testBit word i
            then fn i >> loop (succ i)
            else loop (succ i)
        else return ()

unfoldr :: Bits64 -> Unfoldr Int
unfoldr set =
  Unfoldr (\step acc -> foldr step acc set)

toList :: Bits64 -> [Int]
toList =
  foldr (:) []

-- *

data Location
  = FoundLocation
      !Int
      -- ^ Popcount before.
      !Bits64
      -- ^ Bitmap without this bit.
  | UnfoundLocation
      !Int
      -- ^ Popcount before.
      !Bits64
      -- ^ Bitmap with this bit.

-- |
-- A single function that provides control over virtually all functionality.
{-# INLINE locate #-}
locate :: Int -> Bits64 -> Location
locate idx (Bits64 word) =
  if idx == 0
    then
      let wordWithoutIt = word .&. 0b1111111111111111111111111111111111111111111111111111111111111110
       in if word == wordWithoutIt
            then UnfoundLocation 0 (Bits64 (word .|. 1))
            else FoundLocation 0 (Bits64 wordWithoutIt)
    else
      let bitAtIdx = bit idx
          wordWithIt = word .|. bitAtIdx
          wordWithoutIt = word .&. complement bitAtIdx
          popCountBefore = popCount (word .&. pred bitAtIdx)
       in if word == wordWithIt
            then FoundLocation popCountBefore (Bits64 wordWithoutIt)
            else UnfoundLocation popCountBefore (Bits64 wordWithIt)

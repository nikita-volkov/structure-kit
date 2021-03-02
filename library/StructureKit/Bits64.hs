{-|
Set of 6-bit values represented with a 64-bit word.
-}
module StructureKit.Bits64
(
  Bits64,
  empty,
  null,
  size,
  singleton,
  lookup,
  insert,
  revision,
  split,
  foldr,
  foldl',
  foldlM,
  forM_,
)
where

import StructureKit.Prelude hiding (empty, null, lookup, adjust, insert, split, foldr, foldl', foldlM, forM_)


newtype Bits64 =
  Bits64 Int64

instance Semigroup Bits64 where
  Bits64 l <> Bits64 r = Bits64 (l .|. r)

instance Monoid Bits64 where
  mempty = Bits64 0

empty :: Bits64
empty =
  Bits64 0

null :: Bits64 -> Bool
null (Bits64 word) =
  word == 0

size :: Bits64 -> Int
size (Bits64 word) =
  popCount word

singleton :: Int -> Bits64
singleton value =
  Bits64 (bit value)

lookup :: Int -> Bits64 -> Maybe Int
lookup value (Bits64 word) =
  if value == 0
    then if word .&. 1 /= 0
      then Just 0
      else Nothing
    else let
      bitAtValue = bit value
      isPopulated = bitAtValue .&. word /= 0
      in if isPopulated
        then Just (popCount (word .&. (bitAtValue - 1)))
        else Nothing

insert :: Int -> Bits64 -> (Int, Maybe Bits64)
insert value (Bits64 word) =
  let
    bitAtValue = bit value
    newWord = word .|. bitAtValue
    newBitSet = if newWord == word then Nothing else Just (Bits64 newWord)
    in (fromIntegral bitAtValue, newBitSet)

revision :: Functor f => Int -> (Int -> f Bool) -> (Int -> f Bool) -> Bits64 -> f (Maybe Bits64)
revision value onMissing onPresent (Bits64 word) =
  if value == 0
    then choose 1 0
    else let
      bitAtValue = bit value
      in choose bitAtValue (popCount (word .&. (bitAtValue - 1))) 
  where
    choose bitAtValue populatedIndex =
      if word .&. bitAtValue /= 0
        then
          onPresent populatedIndex
            & fmap (\case
                True -> Just (Bits64 word)
                False -> let
                  newWord = xor word bitAtValue
                  in if newWord == 0
                    then Nothing
                    else Just (Bits64 newWord))
        else
          onMissing populatedIndex
            & fmap (\case
                True -> let
                  newWord = word .|. bitAtValue
                  in Just (Bits64 newWord)
                False -> if word == 0
                  then Nothing
                  else Just (Bits64 word))

{-|
Splits the set in two.
The first one contains all elements smaller than the specified threshold,
the second - all remaining elements.
-}
split :: Int -> Bits64 -> (Bits64, Bits64)
split value (Bits64 word) =
  if value == 0
    then (Bits64 0, Bits64 word)
    else let
      bitAtValue = bit value
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
        then if testBit word i
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
        then if testBit word i
          then step acc i >>= loop (succ i)
          else loop (succ i) acc
        else return acc

forM_ :: Monad m => (Int -> m ()) -> Bits64 -> m ()
forM_ fn (Bits64 word) =
  loop 0
  where
    loop i =
      if i < 64
        then if testBit word i
          then fn i >> loop (succ i)
          else loop (succ i)
        else return ()

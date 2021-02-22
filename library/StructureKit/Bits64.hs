{-|
Set of 6-bit values represented with a 64-bit word.
-}
module StructureKit.Bits64
(
  Bits64,
  empty,
  singleton,
  lookup,
  insert,
  revision,
)
where

import StructureKit.Prelude hiding (empty, lookup, adjust, insert)


newtype Bits64 =
  Bits64 Int64

empty :: Bits64
empty =
  Bits64 0

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

revision :: Functor f => Int -> f Bool -> (Int -> f Bool) -> Bits64 -> f (Maybe Bits64)
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
          onMissing
            & fmap (\case
                True -> let
                  newWord = word .|. bitAtValue
                  in Just (Bits64 newWord)
                False -> if word == 0
                  then Nothing
                  else Just (Bits64 word))

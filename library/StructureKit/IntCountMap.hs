module StructureKit.IntCountMap
(
  IntCountMap,
  empty,
  bump,
  keepLargerSummarizing,
)
where

import StructureKit.Prelude hiding (lookup, insert, empty)
import qualified Data.IntMap.Strict as IntMap


newtype IntCountMap =
  IntCountMap (IntMap Int)

empty :: IntCountMap
empty = IntCountMap IntMap.empty

summarize :: IntCountMap -> Int
summarize (IntCountMap intMap) =
  foldl' (+) 0 intMap

keepLargerSummarizing :: Int -> IntCountMap -> (Int, IntCountMap)
keepLargerSummarizing key (IntCountMap intMap) =
  IntMap.splitLookup key intMap & \(smaller, equal, larger) ->
    (foldl' (+) (fromMaybe 0 equal) smaller,
      IntCountMap larger)

bump :: Int -> IntCountMap -> IntCountMap
bump key (IntCountMap intMap) =
  IntMap.alter (maybe (Just 1) (Just . succ)) key intMap & IntCountMap

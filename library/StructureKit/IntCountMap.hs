module StructureKit.IntCountMap
  ( IntCountMap,
    empty,
    bump,
    keepLargerSummarizing,
  )
where

import qualified Data.IntMap.Strict as IntMap
import StructureKit.Prelude hiding (empty, insert, lookup)

newtype IntCountMap
  = IntCountMap (IntMap Int)

empty :: IntCountMap
empty = IntCountMap IntMap.empty

summarize :: IntCountMap -> Int
summarize (IntCountMap intMap) =
  foldl' (+) 0 intMap

keepLargerSummarizing :: Int -> IntCountMap -> (Int, IntCountMap)
keepLargerSummarizing key (IntCountMap intMap) =
  IntMap.splitLookup key intMap & \(smaller, equal, larger) ->
    ( foldl' (+) (fromMaybe 0 equal) smaller,
      IntCountMap larger
    )

bump :: Int -> IntCountMap -> IntCountMap
bump key (IntCountMap intMap) =
  IntMap.alter (maybe (Just 1) (Just . succ)) key intMap & IntCountMap

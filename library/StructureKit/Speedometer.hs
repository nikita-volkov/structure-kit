module StructureKit.Speedometer
  ( Speedometer,
    empty,
    register,
    sample,
  )
where

import StructureKit.IntCountMap qualified as IntCountMap
import StructureKit.Prelude hiding (empty, insert, lookup)

data Speedometer
  = Speedometer
      IntCountMap.IntCountMap
      -- ^ Count by timestamp.
      Int
      -- ^ Aggregate count.
      --        For fast calculation of speed.
      Int
      -- ^ Memory period.

empty :: Int -> Speedometer
empty = Speedometer IntCountMap.empty 0

register :: Int -> Speedometer -> Speedometer
register timestamp (Speedometer counter aggregate period) =
  Speedometer (IntCountMap.bump timestamp counter) (succ aggregate) period
    & maintain timestamp

maintain :: Int -> Speedometer -> Speedometer
maintain timestamp (Speedometer counter aggregate period) =
  IntCountMap.keepLargerSummarizing (timestamp - period) counter & \(droppedCount, newCounter) ->
    Speedometer newCounter (aggregate - droppedCount) period

calculate :: Speedometer -> Double
calculate (Speedometer map period count) =
  fromIntegral count / fromIntegral period

sample :: Int -> Speedometer -> (Double, Speedometer)
sample timestamp =
  ((,) <$> calculate <*> id) . maintain timestamp

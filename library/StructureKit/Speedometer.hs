module StructureKit.Speedometer
(
  Speedometer,
)
where

import StructureKit.Prelude hiding (lookup, insert, empty)
import qualified Data.IntMap.Strict as IntMap


data Speedometer =
  Speedometer
    {-| Count by timestamp. -}
    (IntMap Int)
    {-| Memory period. -}
    Int

empty :: Int -> Speedometer
empty = Speedometer mempty

register :: Int -> Speedometer -> Speedometer
register timestamp =
  error "TODO"

cleanUp :: Int -> Speedometer -> Speedometer
cleanUp timestamp (Speedometer map period) =
  Speedometer newMap period
  where
    oldestTimestamp =
      timestamp - period
    newMap =
      IntMap.split oldestTimestamp map & snd

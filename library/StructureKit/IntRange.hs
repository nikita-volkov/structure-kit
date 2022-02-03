module StructureKit.IntRange
  ( -- *
    IntRange,
  )
where

import qualified Data.Vector as BVec
import qualified Data.Vector.Unboxed as UVec
import StructureKit.Prelude hiding (empty)

-- *

-- | Composable range of ints.
newtype IntRange
  = IntRange (IntMap Int)

contains :: Int -> IntRange -> Bool
contains val (IntRange map) =
  error "TODO"

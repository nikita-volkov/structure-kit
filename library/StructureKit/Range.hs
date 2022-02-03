module StructureKit.Range
  ( -- *
    FrozenRange,

    -- *
    RangeConstructor,
  )
where

import qualified Data.Vector as BVec
import qualified Data.Vector.Unboxed as UVec
import StructureKit.Prelude hiding (empty)

-- *

data FrozenRange a = FrozenRange
  { beginnings :: UVec a,
    endings :: UVec a
  }

contains :: UVec.Unbox a => a -> FrozenRange a -> Bool
contains a FrozenRange {..} =
  go 0 0
  where
    length = UVec.length beginnings
    go bi ei =
      error "TODO"

-- *

-- | Composable range of primitive values.
data RangeConstructor a

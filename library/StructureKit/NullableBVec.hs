module StructureKit.NullableBVec
  ( NullableBVec,
  )
where

import Data.Vector qualified as BVec
import Data.Vector.Unboxed qualified as UVec
import StructureKit.Prelude hiding (empty)

-- | Optimised alternative to @Vector (Maybe a)@.
data NullableBVec a
  = NullableBVec
      !(UVec Bool)
      !(BVec a)

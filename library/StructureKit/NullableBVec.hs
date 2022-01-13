module StructureKit.NullableBVec
  ( NullableBVec,
  )
where

import qualified Data.Vector as BVec
import qualified Data.Vector.Unboxed as UVec
import StructureKit.Prelude hiding (empty)

-- | Optimised alternative to @Vector (Maybe a)@.
data NullableBVec a
  = NullableBVec
      !(UVec Bool)
      !(BVec a)

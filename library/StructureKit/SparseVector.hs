module StructureKit.SparseVector
  ( -- * --
    SparseVector,
    lookup,
  )
where

import Data.Vector.Generic qualified as GenericVector
import Data.Vector.Unboxed qualified as UnboxedVector
import StructureKit.Prelude hiding (empty, foldr, insert, lookup, read, toList, touch, write)

data SparseVector v a
  = SparseVector
      -- | Null markers.
      !(UnboxedVector.Vector Bool)
      -- | Vector filled with noise in places of missing values.
      !(v a)

lookup :: (GenericVector.Vector v a) => Int -> SparseVector v a -> Maybe a
lookup i (SparseVector nulls elements) =
  case nulls GenericVector.!? i of
    Just True -> Just $ GenericVector.unsafeIndex elements i
    _ -> Nothing

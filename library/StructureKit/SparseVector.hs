module StructureKit.SparseVector
  ( -- *
    SparseVector,
    lookup,
  )
where

import qualified Data.Vector.Generic as GenericVector
import qualified Data.Vector.Unboxed as UnboxedVector
import StructureKit.Prelude hiding (empty, foldr, insert, lookup, read, toList, touch, write)

data SparseVector v a
  = SparseVector
      !(UnboxedVector.Vector Bool)
      -- ^ Null markers.
      !(v a)
      -- ^ Vector filled with noise in places of missing values.

lookup :: GenericVector.Vector v a => Int -> SparseVector v a -> Maybe a
lookup i (SparseVector nulls elements) =
  case nulls GenericVector.!? i of
    Just True -> Just $ GenericVector.unsafeIndex elements i
    _ -> Nothing
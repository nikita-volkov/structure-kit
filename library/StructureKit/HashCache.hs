module StructureKit.HashCache
where

import StructureKit.Prelude
import Data.Primitive.SmallArray (SmallArray)
import qualified StructureKit.Hamt as Hamt


data HashCache k v =
  HashCache
    (Deque k)
    (Hamt.Hamt (Entry k v))

data Entry k v =
  Entry
    {-| Amount of instances in deque. -}
    Int
    k v

module StructureKit.Bits256
  ( Bits256,
    foldr,
  )
where

import StructureKit.Bits64 (Bits64)
import qualified StructureKit.Bits64 as Bits64
import StructureKit.Prelude hiding (adjust, empty, foldl', foldlM, foldr, forM_, insert, lookup, member, null, split, toList, unfoldr)

data Bits256
  = Bits256 Bits64 Bits64 Bits64 Bits64

instance Semigroup Bits256 where
  Bits256 l1 l2 l3 l4 <> Bits256 r1 r2 r3 r4 =
    Bits256 (l1 <> r1) (l2 <> r2) (l3 <> r3) (l4 <> r4)

instance Monoid Bits256 where
  mempty =
    Bits256 mempty mempty mempty mempty

foldr :: (Int -> a -> a) -> a -> Bits256 -> a
foldr step acc (Bits256 a b c d) =
  Bits64.foldr
    step
    ( Bits64.foldr
        step
        ( Bits64.foldr
            step
            (Bits64.foldr step acc d)
            c
        )
        b
    )
    a

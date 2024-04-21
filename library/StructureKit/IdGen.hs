module StructureKit.IdGen
  ( IdGen,
    empty,
    release,
    fetch,
  )
where

import Data.IntSet qualified as IntSet
import StructureKit.Prelude hiding (empty)

data IdGen
  = IdGen
      !IntSet
      !Int

instance Semigroup IdGen where
  IdGen lReleased lCounter <> IdGen rReleased rCounter =
    IdGen (lReleased <> rReleased) (max lCounter rCounter)

instance Monoid IdGen where
  mempty = empty

{-# INLINE empty #-}
empty :: IdGen
empty = IdGen mempty 1

{-# INLINE release #-}
release :: Int -> IdGen -> IdGen
release key (IdGen released counter) = IdGen (IntSet.insert key released) counter

{-# INLINE fetch #-}
fetch :: IdGen -> (Int, IdGen)
fetch (IdGen released counter) =
  case IntSet.minView released of
    Nothing -> (counter, IdGen released (succ counter))
    Just (id, newReleased) -> (id, IdGen newReleased counter)

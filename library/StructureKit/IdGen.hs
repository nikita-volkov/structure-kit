module StructureKit.IdGen
  ( IdGen,
    empty,
    release,
    fetch,
  )
where

import qualified Deque.Strict as Deque
import StructureKit.Prelude hiding (empty)

data IdGen
  = IdGen
      (Deque Int)
      Int

instance Semigroup IdGen where
  IdGen lDeque lCounter <> IdGen rDeque rCounter =
    IdGen (lDeque <> rDeque) (max lCounter rCounter)

instance Monoid IdGen where
  mempty = empty

empty :: IdGen
empty = IdGen mempty 1

release :: Int -> IdGen -> IdGen
release key (IdGen released counter) = IdGen (Deque.snoc key released) counter

fetch :: IdGen -> (Int, IdGen)
fetch (IdGen released counter) =
  case Deque.uncons released of
    Just (id, newReturned) -> (id, IdGen newReturned counter)
    Nothing -> (counter, IdGen released (succ counter))

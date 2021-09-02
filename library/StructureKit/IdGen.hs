module StructureKit.IdGen
(
  IdGen,
  empty,
  release,
  fetch,
)
where

import StructureKit.Prelude hiding (empty)
import qualified Deque.Strict as Deque


data IdGen =
  IdGen
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
release key (IdGen returned counter) = IdGen (Deque.snoc key returned) counter

fetch :: IdGen -> (Int, IdGen)
fetch (IdGen returned counter) =
  case Deque.uncons returned of
    Just (id, newReturned) -> (id, IdGen newReturned counter)
    Nothing -> (counter, IdGen returned (succ counter))

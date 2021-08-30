module StructureKit.KeyRegistry
(
  KeyRegistry,
  empty,
  release,
  fetch,
)
where

import StructureKit.Prelude hiding (empty)
import qualified Deque.Strict as Deque


data KeyRegistry =
  KeyRegistry
    (Deque Int)
    Int

instance Semigroup KeyRegistry where
  KeyRegistry lDeque lCounter <> KeyRegistry rDeque rCounter =
    KeyRegistry (lDeque <> rDeque) (max lCounter rCounter)

instance Monoid KeyRegistry where
  mempty = empty

empty :: KeyRegistry
empty = KeyRegistry mempty 1

release :: Int -> KeyRegistry -> KeyRegistry
release key (KeyRegistry returned counter) = KeyRegistry (Deque.snoc key returned) counter

fetch :: KeyRegistry -> (Int, KeyRegistry)
fetch (KeyRegistry returned counter) =
  case Deque.uncons returned of
    Just (id, newReturned) -> (id, KeyRegistry newReturned counter)
    Nothing -> (counter, KeyRegistry returned (succ counter))

module StructureKit.KeyRegistry
(
  KeyRegistry,
  empty,
  return,
  fetch,
)
where

import StructureKit.Prelude hiding (return, empty)
import qualified Deque.Strict as Deque


data KeyRegistry =
  KeyRegistry
    (Deque Int)
    Int

empty :: KeyRegistry
empty = KeyRegistry mempty 1

return :: Int -> KeyRegistry -> KeyRegistry
return key (KeyRegistry returned counter) = KeyRegistry (Deque.snoc key returned) counter

fetch :: KeyRegistry -> (Int, KeyRegistry)
fetch (KeyRegistry returned counter) =
  case Deque.uncons returned of
    Just (id, newReturned) -> (id, KeyRegistry newReturned counter)
    Nothing -> (counter, KeyRegistry returned (succ counter))

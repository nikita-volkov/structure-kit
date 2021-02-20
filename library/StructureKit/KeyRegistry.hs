module StructureKit.KeyRegistry
(
  KeyRegistry,
  init,
  return,
  fetch,
)
where

import StructureKit.Prelude hiding (return, init)
import qualified Deque.Strict as Deque


data KeyRegistry =
  KeyRegistry
    (Deque Int)
    Int

init :: KeyRegistry
init = KeyRegistry mempty 1

return :: Int -> KeyRegistry -> KeyRegistry
return key (KeyRegistry returned counter) = KeyRegistry (Deque.snoc key returned) counter

fetch :: KeyRegistry -> (Int, KeyRegistry)
fetch (KeyRegistry returned counter) =
  case Deque.uncons returned of
    Just (id, newReturned) -> (id, KeyRegistry newReturned counter)
    Nothing -> (counter, KeyRegistry returned (succ counter))

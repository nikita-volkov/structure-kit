module StructureKit.IdRegistry
  ( IdRegistry,
    empty,
    register,
    unregister,
    lookup,
  )
where

import qualified Data.IntMap.Strict as IntMap
import qualified StructureKit.IdGen as IdGen
import StructureKit.Prelude hiding (empty, lookup)

data IdRegistry a
  = IdRegistry
      IdGen.IdGen
      (IntMap a)
  deriving (Functor, Foldable, Traversable)

empty :: IdRegistry a
empty = IdRegistry IdGen.empty IntMap.empty

register :: a -> IdRegistry a -> (Int, IdRegistry a)
register payload (IdRegistry gen map) =
  let (id, newGen) = IdGen.fetch gen
      newMap = IntMap.insert id payload map
      newRegistry = IdRegistry newGen newMap
   in (id, newRegistry)

unregister :: Int -> IdRegistry a -> (Bool, IdRegistry a)
unregister = error "TODO"

lookup :: Int -> IdRegistry a -> Maybe a
lookup = error "TODO"

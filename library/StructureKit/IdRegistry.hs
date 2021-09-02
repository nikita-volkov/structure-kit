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

-- |
-- Registry of values that automatically generates keys.
data IdRegistry a
  = IdRegistry
      IdGen.IdGen
      (IntMap a)
  deriving (Functor, Foldable, Traversable)

-- |
-- Create an empty registry.
empty :: IdRegistry a
empty = IdRegistry IdGen.empty IntMap.empty

-- |
-- Register a value and produce a key that can be later used to retrieve this value.
register :: a -> IdRegistry a -> (Int, IdRegistry a)
register payload (IdRegistry gen map) =
  let (id, newGen) = IdGen.fetch gen
      newMap = IntMap.insert id payload map
      newRegistry = IdRegistry newGen newMap
   in (id, newRegistry)

-- |
-- Unregister an entry by its key.
--
-- Produces the value of the entry if it has been previously registered.
unregister :: Int -> IdRegistry a -> (Maybe a, IdRegistry a)
unregister id (IdRegistry gen map) =
  case IntMap.alterF (maybe (Nothing, Nothing) (\a -> (Just a, Nothing))) id map of
    (deleted, newMap) ->
      let newGen = if isNothing deleted then gen else IdGen.release id gen
          newRegistry = IdRegistry newGen newMap
       in (deleted, newRegistry)

-- |
-- Lookup an entry by its key.
lookup :: Int -> IdRegistry a -> Maybe a
lookup id (IdRegistry _ map) =
  IntMap.lookup id map

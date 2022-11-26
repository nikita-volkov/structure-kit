module StructureKit.IdRegistry
  ( IdRegistry,
    empty,
    register,
    unregister,
    lookup,
    adjust,
    alterF,
    toAssocList,
  )
where

import Data.IntMap.Strict qualified as IntMap
import StructureKit.IdGen qualified as IdGen
import StructureKit.Prelude hiding (empty, lookup)

-- |
-- Registry of values that automatically generates keys.
data IdRegistry a
  = IdRegistry
      {-# UNPACK #-} !IdGen.IdGen
      !(IntMap a)
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

-- |
-- Same operation as 'IntMap.alterF'.
alterF :: Functor f => (Maybe a -> f (Maybe a)) -> Int -> IdRegistry a -> f (IdRegistry a)
alterF f k (IdRegistry idGen map) =
  IntMap.alterF f k map & fmap (IdRegistry idGen)

-- |
-- Same operation as 'IntMap.adjust'.
adjust :: (a -> a) -> Int -> IdRegistry a -> IdRegistry a
adjust f k (IdRegistry idGen map) =
  IdRegistry idGen (IntMap.adjust f k map)

-- |
-- Convert to a list of keys and values.
toAssocList :: IdRegistry a -> [(Int, a)]
toAssocList (IdRegistry _ map) = IntMap.toList map

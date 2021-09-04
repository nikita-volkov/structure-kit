module StructureKit.Hamt
  ( -- *
    Hamt,
    empty,
    findMapping,
    findAndReplace,
    revise,
    delete,
    null,

    -- * Selection API
    select,

    -- ** Present
    Present,
    read,
    remove,
    overwrite,

    -- ** Missing
    Missing,
    write,
  )
where

import Data.Primitive.SmallArray (SmallArray)
import qualified Data.Primitive.SmallArray as SmallArray
import qualified StructureKit.By32Bits as By32Bits
import StructureKit.Prelude hiding (delete, empty, null, read, remove, write)
import qualified StructureKit.Util.SmallArray as SmallArray

newtype Hamt a
  = Hamt (By32Bits.By32Bits (SmallArray a))

empty :: Hamt a
empty =
  Hamt (By32Bits.empty)

findMapping :: Int -> (a -> Maybe b) -> Hamt a -> Maybe b
findMapping hash cont (Hamt map) =
  By32Bits.lookup hash map >>= SmallArray.findMapping cont

findAndReplace :: Int -> (a -> Maybe a) -> Hamt a -> (Maybe a, Hamt a)
findAndReplace hash narrow hamt =
  case select hash (isJust . narrow) hamt of
    Right present ->
      case narrow (read present) of
        Just newA -> (Just (read present), overwrite newA present)
        Nothing -> error "Oops"
    Left missing ->
      (Nothing, hamt)

-- |
-- Deprecated.
revise :: Functor f => Int -> (a -> Maybe b) -> f (Maybe a) -> (b -> f (Maybe a)) -> Hamt a -> f (Maybe (Hamt a))
revise hash narrow onMissing onPresent hamt =
  case select hash (isJust . narrow) hamt of
    Right present ->
      case narrow (read present) of
        Just b ->
          onPresent b <&> \case
            Just newA ->
              overwrite newA present & Just
            Nothing ->
              remove present & guarded (not . null)
        Nothing ->
          error "Oops"
    Left missing ->
      onMissing <&> \case
        Just a -> write a missing & Just
        Nothing -> Just hamt

-- |
-- Deprecated.
delete :: Int -> (a -> Maybe b) -> Hamt a -> (Maybe b, Maybe (Hamt a))
delete hash narrow hamt =
  case select hash (isJust . narrow) hamt of
    Right present ->
      (narrow (read present), guarded (not . null) (remove present))
    Left missing ->
      (Nothing, Just hamt)

null :: Hamt a -> Bool
null (Hamt map) = By32Bits.null map

-- * Selection API

select :: Int -> (a -> Bool) -> Hamt a -> Either (Missing a) (Present a)
select hash predicate (Hamt map) =
  case By32Bits.select hash map of
    Right mapPresent ->
      let array = By32Bits.read mapPresent
       in case SmallArray.findWithIndex predicate array of
            Just (idx, val) ->
              let without =
                    let newArray = SmallArray.unset idx array
                     in if SmallArray.null newArray
                          then Hamt $ By32Bits.remove mapPresent
                          else Hamt $ By32Bits.overwrite newArray mapPresent
                  overwrite val =
                    let newArray = SmallArray.set idx val array
                     in Hamt $ By32Bits.overwrite newArray mapPresent
               in Right $ Present val without overwrite
            Nothing ->
              let write val =
                    let newArray = SmallArray.snoc val array
                     in Hamt $ By32Bits.overwrite newArray mapPresent
               in Left $ Missing write
    Left mapMissing ->
      let write val =
            let newArray = SmallArray.singleton val
             in Hamt $ By32Bits.write newArray mapMissing
       in Left $ Missing write

-- **

data Present a
  = Present ~a ~(Hamt a) (a -> Hamt a)

read :: Present a -> a
read (Present x _ _) = x

remove :: Present a -> Hamt a
remove (Present _ x _) = x

overwrite :: a -> Present a -> Hamt a
overwrite val (Present _ _ x) = x val

-- **

newtype Missing a
  = Missing (a -> Hamt a)

write :: a -> Missing a -> Hamt a
write val (Missing x) = x val

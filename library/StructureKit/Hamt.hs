module StructureKit.Hamt
  ( -- * --
    Hamt,
    empty,
    findMapping,
    findAndReplace,
    revise,
    delete,
    null,

    -- * Location API
    locate,

    -- ** Existing
    Existing,
    read,
    remove,
    overwrite,

    -- ** Missing
    Missing,
    write,
  )
where

import StructureKit.By32Bits qualified as By32Bits
import StructureKit.Prelude hiding (delete, empty, null, read, remove, write)
import StructureKit.Util.SmallArray qualified as SmallArray

newtype Hamt a
  = Hamt (By32Bits.By32Bits (SmallArray a))
  deriving (Functor, NFData)

empty :: Hamt a
empty =
  Hamt (By32Bits.empty)

findMapping :: Int -> (a -> Maybe b) -> Hamt a -> Maybe b
findMapping hash cont (Hamt map) =
  By32Bits.lookup hash map >>= SmallArray.findMapping cont

findAndReplace :: Int -> (a -> Maybe a) -> Hamt a -> (Maybe a, Hamt a)
findAndReplace hash narrow hamt =
  case locate hash (isJust . narrow) hamt of
    Right existing ->
      case narrow (read existing) of
        Just newA -> (Just (read existing), overwrite newA existing)
        Nothing -> error "Oops"
    Left missing ->
      (Nothing, hamt)

-- |
-- Deprecated.
revise :: (Functor f) => Int -> (a -> Maybe b) -> f (Maybe a) -> (b -> f (Maybe a)) -> Hamt a -> f (Maybe (Hamt a))
revise hash narrow onMissing onPresent hamt =
  case locate hash (isJust . narrow) hamt of
    Right existing ->
      case narrow (read existing) of
        Just b ->
          onPresent b <&> \case
            Just newA ->
              overwrite newA existing & Just
            Nothing ->
              remove existing & guarded (not . null)
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
  case locate hash (isJust . narrow) hamt of
    Right existing ->
      (narrow (read existing), guarded (not . null) (remove existing))
    Left missing ->
      (Nothing, Just hamt)

null :: Hamt a -> Bool
null (Hamt map) = By32Bits.null map

-- * Location API

{-# INLINE locate #-}
locate :: Int -> (a -> Bool) -> Hamt a -> Either (Missing a) (Existing a)
locate hash predicate (Hamt map) =
  {-# SCC "locate" #-}
  case By32Bits.locate hash map of
    Right arrayExisting ->
      case By32Bits.read arrayExisting of
        array ->
          case SmallArray.locateByPredicate predicate array of
            Just elementExisting ->
              Right $ Existing arrayExisting elementExisting
            Nothing ->
              Left $ ElementMissing arrayExisting array
    Left arrayMissing ->
      Left $ ArrayMissing arrayMissing

-- ** --

data Existing a
  = Existing
      !(By32Bits.Existing (SmallArray a))
      !(SmallArray.Existing a)

read :: Existing a -> a
read (Existing _ b) = SmallArray.read b

remove :: Existing a -> Hamt a
remove (Existing a b) =
  SmallArray.remove b & \b ->
    if SmallArray.null b
      then By32Bits.remove a & Hamt
      else By32Bits.overwrite b a & Hamt

overwrite :: a -> Existing a -> Hamt a
overwrite val (Existing a b) =
  val
    & flip SmallArray.overwrite b
    & flip By32Bits.overwrite a
    & Hamt

-- ** --

data Missing a
  = ArrayMissing
      !(By32Bits.Missing (SmallArray a))
  | ElementMissing
      !(By32Bits.Existing (SmallArray a))
      !(SmallArray a)

write :: a -> Missing a -> Hamt a
write val = \case
  ArrayMissing a ->
    val
      & SmallArray.singleton
      & flip By32Bits.write a
      & Hamt
  ElementMissing a b ->
    val
      & flip SmallArray.cons b
      & flip By32Bits.overwrite a
      & Hamt

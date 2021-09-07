module StructureKit.By8Bits
  ( -- *
    By8Bits,
    empty,
    singleton,
    lookup,
    insert,
    adjust,
    foldrWithKey,
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

import qualified StructureKit.By6Bits as By6Bits
import StructureKit.Prelude hiding (empty, insert, lookup, null, read, remove, singleton, write)

-- |
-- Map indexed with 8 bits.
data By8Bits a
  = By8Bits (By6Bits.By6Bits a) (By6Bits.By6Bits a) (By6Bits.By6Bits a) (By6Bits.By6Bits a)

empty :: By8Bits a
empty =
  By8Bits By6Bits.empty By6Bits.empty By6Bits.empty By6Bits.empty

singleton :: Int -> a -> By8Bits a
singleton key val =
  if key < 128
    then
      if key < 64
        then By8Bits (By6Bits.singleton key val) By6Bits.empty By6Bits.empty By6Bits.empty
        else By8Bits By6Bits.empty (By6Bits.singleton (key - 64) val) By6Bits.empty By6Bits.empty
    else
      if key < 192
        then By8Bits By6Bits.empty By6Bits.empty (By6Bits.singleton (key - 128) val) By6Bits.empty
        else By8Bits By6Bits.empty By6Bits.empty By6Bits.empty (By6Bits.singleton (key - 192) val)

lookup :: Int -> By8Bits a -> Maybe a
lookup key (By8Bits a b c d) =
  By6Bits.lookup
    (error "TODO")
    if key < 128
      then
        if key < 64
          then a
          else b
      else
        if key < 192
          then c
          else d

insert :: Int -> a -> By8Bits a -> (Maybe a, By8Bits a)
insert key value (By8Bits a b c d) =
  if key < 128
    then
      if key < 64
        then By6Bits.insert key value a & second (\a -> By8Bits a b c d)
        else By6Bits.insert (error "TODO") value b & second (\b -> By8Bits a b c d)
    else
      if key < 192
        then By6Bits.insert (error "TODO") value c & second (\c -> By8Bits a b c d)
        else By6Bits.insert (error "TODO") value d & second (\d -> By8Bits a b c d)

adjust :: (a -> a) -> Int -> By8Bits a -> By8Bits a
adjust =
  error "TODO"

foldrWithKey :: (Int -> a -> b -> b) -> b -> By8Bits a -> b
foldrWithKey step end (By8Bits part1 part2 part3 part4) =
  By6Bits.foldrWithKey
    step
    ( By6Bits.foldrWithKey
        (offsetStep 64)
        ( By6Bits.foldrWithKey
            (offsetStep 128)
            ( By6Bits.foldrWithKey
                (offsetStep 192)
                end
                part4
            )
            part3
        )
        part2
    )
    part1
  where
    offsetStep offset key =
      step (key + offset)

null :: By8Bits a -> Bool
null (By8Bits a b c d) =
  By6Bits.null a && By6Bits.null b && By6Bits.null c && By6Bits.null d

-- * Location API

locate :: Int -> By8Bits a -> Either (Missing a) (Existing a)
locate key (By8Bits a b c d) =
  if key < 128
    then
      if key < 64
        then case By6Bits.locate key a of
          Right present ->
            Right $
              Existing
                (By6Bits.read present)
                (By8Bits (By6Bits.remove present) b c d)
                (\val -> By8Bits (By6Bits.overwrite val present) b c d)
          Left missing ->
            Left $
              Missing
                (\val -> By8Bits (By6Bits.write val missing) b c d)
        else case By6Bits.locate (key - 64) b of
          Right present ->
            Right $
              Existing
                (By6Bits.read present)
                (By8Bits a (By6Bits.remove present) c d)
                (\val -> By8Bits a (By6Bits.overwrite val present) c d)
          Left missing ->
            Left $
              Missing
                (\val -> By8Bits a (By6Bits.write val missing) c d)
    else
      if key < 192
        then case By6Bits.locate (key - 128) c of
          Right present ->
            Right $
              Existing
                (By6Bits.read present)
                (By8Bits a b (By6Bits.remove present) d)
                (\val -> By8Bits a b (By6Bits.overwrite val present) d)
          Left missing ->
            Left $
              Missing
                (\val -> By8Bits a b (By6Bits.write val missing) d)
        else case By6Bits.locate (key - 192) d of
          Right present ->
            Right $
              Existing
                (By6Bits.read present)
                (By8Bits a b c (By6Bits.remove present))
                (\val -> By8Bits a b c (By6Bits.overwrite val present))
          Left missing ->
            Left $
              Missing
                (\val -> By8Bits a b c (By6Bits.write val missing))

-- **

data Existing a
  = Existing a (By8Bits a) (a -> By8Bits a)

read :: Existing a -> a
read (Existing x _ _) = x

remove :: Existing a -> By8Bits a
remove (Existing _ x _) = x

overwrite :: a -> Existing a -> By8Bits a
overwrite val (Existing _ _ x) = x val

-- **

newtype Missing a
  = Missing (a -> By8Bits a)

write :: a -> Missing a -> By8Bits a
write val (Missing x) = x val

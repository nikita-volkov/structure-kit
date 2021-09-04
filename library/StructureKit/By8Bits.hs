module StructureKit.By8Bits
  ( By8Bits,
    empty,
    lookup,
    insert,
    adjust,
    foldrWithKey,
  )
where

import qualified StructureKit.By6Bits as By6Bits
import StructureKit.Prelude hiding (empty, insert, lookup)

-- |
-- Map indexed with 8 bits.
data By8Bits a
  = By8Bits (By6Bits.By6Bits a) (By6Bits.By6Bits a) (By6Bits.By6Bits a) (By6Bits.By6Bits a)

empty :: By8Bits a
empty =
  By8Bits By6Bits.empty By6Bits.empty By6Bits.empty By6Bits.empty

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

-- * Selection API

select :: Int -> By8Bits a -> Either (Missing a) (Present a)
select key (By8Bits a b c d) =
  if key < 128
    then
      if key < 64
        then case By6Bits.select key a of
          Right present -> Right $ APresent present b c d
          Left missing -> Left $ AMissing missing b c d
        else case By6Bits.select (key - 64) b of
          Right present -> Right $ BPresent a present c d
          Left missing -> Left $ BMissing a missing c d
    else
      if key < 192
        then case By6Bits.select (key - 128) c of
          Right present -> Right $ CPresent a b present d
          Left missing -> Left $ CMissing a b missing d
        else case By6Bits.select (key - 192) d of
          Right present -> Right $ DPresent a b c present
          Left missing -> Left $ DMissing a b c missing

-- **

data Present a
  = APresent (By6Bits.Present a) (By6Bits.By6Bits a) (By6Bits.By6Bits a) (By6Bits.By6Bits a)
  | BPresent (By6Bits.By6Bits a) (By6Bits.Present a) (By6Bits.By6Bits a) (By6Bits.By6Bits a)
  | CPresent (By6Bits.By6Bits a) (By6Bits.By6Bits a) (By6Bits.Present a) (By6Bits.By6Bits a)
  | DPresent (By6Bits.By6Bits a) (By6Bits.By6Bits a) (By6Bits.By6Bits a) (By6Bits.Present a)

read :: Present a -> a
read = \case
  APresent present b c d ->
    By6Bits.read present

remove :: Present a -> By8Bits a
remove = \case
  APresent present b c d ->
    By8Bits (By6Bits.remove present) b c d

overwrite :: a -> Present a -> By8Bits a
overwrite =
  error "TODO"

-- **

data Missing a
  = AMissing (By6Bits.Missing a) (By6Bits.By6Bits a) (By6Bits.By6Bits a) (By6Bits.By6Bits a)
  | BMissing (By6Bits.By6Bits a) (By6Bits.Missing a) (By6Bits.By6Bits a) (By6Bits.By6Bits a)
  | CMissing (By6Bits.By6Bits a) (By6Bits.By6Bits a) (By6Bits.Missing a) (By6Bits.By6Bits a)
  | DMissing (By6Bits.By6Bits a) (By6Bits.By6Bits a) (By6Bits.By6Bits a) (By6Bits.Missing a)

write :: a -> Missing a -> By8Bits a
write =
  error "TODO"

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
  = By8Bits
      !(By6Bits.By6Bits a)
      !(By6Bits.By6Bits a)
      !(By6Bits.By6Bits a)
      !(By6Bits.By6Bits a)
  deriving (Functor)

instance NFData a => NFData (By8Bits a) where
  rnf (By8Bits a b c d) = a `deepseq` b `deepseq` c `deepseq` d `seq` ()

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

{-# INLINE null #-}
null :: By8Bits a -> Bool
null (By8Bits a b c d) =
  By6Bits.null a && By6Bits.null b && By6Bits.null c && By6Bits.null d

-- * Location API

{-# INLINE locate #-}
locate :: Int -> By8Bits a -> Either (Missing a) (Existing a)
locate key (By8Bits a b c d) =
  {-# SCC "locate" #-}
  if key < 128
    then
      if key < 64
        then case By6Bits.locate key a of
          Right x -> Right $ AExisting x b c d
          Left x -> Left $ AMissing x b c d
        else case By6Bits.locate (key - 64) b of
          Right x -> Right $ BExisting a x c d
          Left x -> Left $ BMissing a x c d
    else
      if key < 192
        then case By6Bits.locate (key - 128) c of
          Right x -> Right $ CExisting a b x d
          Left x -> Left $ CMissing a b x d
        else case By6Bits.locate (key - 192) d of
          Right x -> Right $ DExisting a b c x
          Left x -> Left $ DMissing a b c x

-- **

data Existing a
  = AExisting
      !(By6Bits.Existing a)
      !(By6Bits.By6Bits a)
      !(By6Bits.By6Bits a)
      !(By6Bits.By6Bits a)
  | BExisting
      !(By6Bits.By6Bits a)
      !(By6Bits.Existing a)
      !(By6Bits.By6Bits a)
      !(By6Bits.By6Bits a)
  | CExisting
      !(By6Bits.By6Bits a)
      !(By6Bits.By6Bits a)
      !(By6Bits.Existing a)
      !(By6Bits.By6Bits a)
  | DExisting
      !(By6Bits.By6Bits a)
      !(By6Bits.By6Bits a)
      !(By6Bits.By6Bits a)
      !(By6Bits.Existing a)

read :: Existing a -> a
read = \case
  AExisting x _ _ _ -> By6Bits.read x
  BExisting _ x _ _ -> By6Bits.read x
  CExisting _ _ x _ -> By6Bits.read x
  DExisting _ _ _ x -> By6Bits.read x

remove :: Existing a -> By8Bits a
remove = \case
  AExisting x b c d -> By8Bits (By6Bits.remove x) b c d
  BExisting a x c d -> By8Bits a (By6Bits.remove x) c d
  CExisting a b x d -> By8Bits a b (By6Bits.remove x) d
  DExisting a b c x -> By8Bits a b c (By6Bits.remove x)

overwrite :: a -> Existing a -> By8Bits a
overwrite val = \case
  AExisting x b c d -> By8Bits (By6Bits.overwrite val x) b c d
  BExisting a x c d -> By8Bits a (By6Bits.overwrite val x) c d
  CExisting a b x d -> By8Bits a b (By6Bits.overwrite val x) d
  DExisting a b c x -> By8Bits a b c (By6Bits.overwrite val x)

-- **

data Missing a
  = AMissing
      !(By6Bits.Missing a)
      !(By6Bits.By6Bits a)
      !(By6Bits.By6Bits a)
      !(By6Bits.By6Bits a)
  | BMissing
      !(By6Bits.By6Bits a)
      !(By6Bits.Missing a)
      !(By6Bits.By6Bits a)
      !(By6Bits.By6Bits a)
  | CMissing
      !(By6Bits.By6Bits a)
      !(By6Bits.By6Bits a)
      !(By6Bits.Missing a)
      !(By6Bits.By6Bits a)
  | DMissing
      !(By6Bits.By6Bits a)
      !(By6Bits.By6Bits a)
      !(By6Bits.By6Bits a)
      !(By6Bits.Missing a)

write :: a -> Missing a -> By8Bits a
write val = \case
  AMissing x b c d -> By8Bits (By6Bits.write val x) b c d
  BMissing a x c d -> By8Bits a (By6Bits.write val x) c d
  CMissing a b x d -> By8Bits a b (By6Bits.write val x) d
  DMissing a b c x -> By8Bits a b c (By6Bits.write val x)

-- |
-- Map indexed by 32 bits.
module StructureKit.By32Bits
  ( -- * --
    By32Bits,
    empty,
    lookup,
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

import StructureKit.By32Bits.KeyOps qualified as KeyOps
import StructureKit.By6Bits (By6Bits)
import StructureKit.By6Bits qualified as By6Bits
import StructureKit.By8Bits (By8Bits)
import StructureKit.By8Bits qualified as By8Bits
import StructureKit.Prelude hiding (adjust, empty, lookup, null, read, remove, write)

newtype By32Bits a
  = By32Bits (By6Bits (By6Bits (By6Bits (By6Bits (By8Bits a)))))
  deriving (NFData, Functor)

empty :: By32Bits a
empty =
  By32Bits By6Bits.empty

-- |
-- Lookup only using the first 32 bits.
lookup :: Int -> By32Bits a -> Maybe a
lookup key =
  either (const Nothing) (Just . read) . locate key

null :: By32Bits a -> Bool
null (By32Bits tree1) = By6Bits.null tree1

-- * Location API

{-# INLINE locate #-}
locate :: Int -> By32Bits a -> Either (Missing a) (Existing a)
locate key (By32Bits tree1) =
  {-# SCC "locate" #-}
  case By6Bits.locate key1 tree1 of
    Right existing1 -> case By6Bits.locate key2 (By6Bits.read existing1) of
      Right existing2 -> case By6Bits.locate key3 (By6Bits.read existing2) of
        Right existing3 -> case By6Bits.locate key4 (By6Bits.read existing3) of
          Right existing4 -> case By8Bits.locate key5 (By6Bits.read existing4) of
            Right existing5 -> Right $ Existing existing1 existing2 existing3 existing4 existing5
            Left missing5 -> Left $ At5Missing existing1 existing2 existing3 existing4 missing5
          Left missing4 -> Left $ At4Missing existing1 existing2 existing3 missing4 key5
        Left missing3 -> Left $ At3Missing existing1 existing2 missing3 key4 key5
      Left missing2 -> Left $ At2Missing existing1 missing2 key3 key4 key5
    Left missing1 -> Left $ At1Missing missing1 key2 key3 key4 key5
  where
    !key1 = KeyOps.toIndexOfLevel1 key
    !key2 = KeyOps.toIndexOfLevel2 key
    !key3 = KeyOps.toIndexOfLevel3 key
    !key4 = KeyOps.toIndexOfLevel4 key
    !key5 = KeyOps.toIndexOfLevel5 key

-- ** --

data Existing a
  = Existing
      {-# UNPACK #-} !(By6Bits.Existing (By6Bits (By6Bits (By6Bits (By8Bits a)))))
      {-# UNPACK #-} !(By6Bits.Existing (By6Bits (By6Bits (By8Bits a))))
      {-# UNPACK #-} !(By6Bits.Existing (By6Bits (By8Bits a)))
      {-# UNPACK #-} !(By6Bits.Existing (By8Bits a))
      !(By8Bits.Existing a)

{-# INLINE read #-}
read :: Existing a -> a
read (Existing _ _ _ _ e) =
  By8Bits.read e

remove :: Existing a -> By32Bits a
remove (Existing a b c d e) =
  case By8Bits.remove e of
    e ->
      if By8Bits.null e
        then case By6Bits.remove d of
          d ->
            if By6Bits.null d
              then case By6Bits.remove c of
                c ->
                  if By6Bits.null c
                    then case By6Bits.remove b of
                      b ->
                        if By6Bits.null b
                          then case By6Bits.remove a of
                            a ->
                              a & By32Bits
                          else
                            b & flip By6Bits.overwrite a
                              & By32Bits
                    else
                      c & flip By6Bits.overwrite b
                        & flip By6Bits.overwrite a
                        & By32Bits
              else
                d & flip By6Bits.overwrite c
                  & flip By6Bits.overwrite b
                  & flip By6Bits.overwrite a
                  & By32Bits
        else
          e & flip By6Bits.overwrite d
            & flip By6Bits.overwrite c
            & flip By6Bits.overwrite b
            & flip By6Bits.overwrite a
            & By32Bits

overwrite :: a -> Existing a -> By32Bits a
overwrite val (Existing a b c d e) =
  val
    & flip By8Bits.overwrite e
    & flip By6Bits.overwrite d
    & flip By6Bits.overwrite c
    & flip By6Bits.overwrite b
    & flip By6Bits.overwrite a
    & By32Bits

-- ** --

data Missing a
  = At1Missing
      {-# UNPACK #-} !(By6Bits.Missing (By6Bits (By6Bits (By6Bits (By8Bits a)))))
      {-# UNPACK #-} !Int
      {-# UNPACK #-} !Int
      {-# UNPACK #-} !Int
      {-# UNPACK #-} !Int
  | At2Missing
      {-# UNPACK #-} !(By6Bits.Existing (By6Bits (By6Bits (By6Bits (By8Bits a)))))
      {-# UNPACK #-} !(By6Bits.Missing (By6Bits (By6Bits (By8Bits a))))
      {-# UNPACK #-} !Int
      {-# UNPACK #-} !Int
      {-# UNPACK #-} !Int
  | At3Missing
      {-# UNPACK #-} !(By6Bits.Existing (By6Bits (By6Bits (By6Bits (By8Bits a)))))
      {-# UNPACK #-} !(By6Bits.Existing (By6Bits (By6Bits (By8Bits a))))
      {-# UNPACK #-} !(By6Bits.Missing (By6Bits (By8Bits a)))
      {-# UNPACK #-} !Int
      {-# UNPACK #-} !Int
  | At4Missing
      {-# UNPACK #-} !(By6Bits.Existing (By6Bits (By6Bits (By6Bits (By8Bits a)))))
      {-# UNPACK #-} !(By6Bits.Existing (By6Bits (By6Bits (By8Bits a))))
      {-# UNPACK #-} !(By6Bits.Existing (By6Bits (By8Bits a)))
      {-# UNPACK #-} !(By6Bits.Missing (By8Bits a))
      {-# UNPACK #-} !Int
  | At5Missing
      {-# UNPACK #-} !(By6Bits.Existing (By6Bits (By6Bits (By6Bits (By8Bits a)))))
      {-# UNPACK #-} !(By6Bits.Existing (By6Bits (By6Bits (By8Bits a))))
      {-# UNPACK #-} !(By6Bits.Existing (By6Bits (By8Bits a)))
      {-# UNPACK #-} !(By6Bits.Existing (By8Bits a))
      !(By8Bits.Missing a)

write :: a -> Missing a -> By32Bits a
write val = \case
  At1Missing x1 k2 k3 k4 k5 ->
    By8Bits.singleton k5 val
      & By6Bits.singleton k4
      & By6Bits.singleton k3
      & By6Bits.singleton k2
      & flip By6Bits.write x1
      & By32Bits
  At2Missing x1 x2 k3 k4 k5 ->
    By8Bits.singleton k5 val
      & By6Bits.singleton k4
      & By6Bits.singleton k3
      & flip By6Bits.write x2
      & flip By6Bits.overwrite x1
      & By32Bits
  At3Missing x1 x2 x3 k4 k5 ->
    By8Bits.singleton k5 val
      & By6Bits.singleton k4
      & flip By6Bits.write x3
      & flip By6Bits.overwrite x2
      & flip By6Bits.overwrite x1
      & By32Bits
  At4Missing x1 x2 x3 x4 k5 ->
    By8Bits.singleton k5 val
      & flip By6Bits.write x4
      & flip By6Bits.overwrite x3
      & flip By6Bits.overwrite x2
      & flip By6Bits.overwrite x1
      & By32Bits
  At5Missing x1 x2 x3 x4 x5 ->
    By8Bits.write val x5
      & flip By6Bits.overwrite x4
      & flip By6Bits.overwrite x3
      & flip By6Bits.overwrite x2
      & flip By6Bits.overwrite x1
      & By32Bits

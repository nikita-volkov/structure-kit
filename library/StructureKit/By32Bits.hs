-- |
-- Map indexed by 32 bits.
module StructureKit.By32Bits
  ( -- *
    By32Bits,
    empty,
    lookup,
    adjust,
    mapAt,
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

import qualified StructureKit.By32Bits.KeyOps as KeyOps
import StructureKit.By6Bits (By6Bits)
import qualified StructureKit.By6Bits as By6Bits
import StructureKit.By8Bits (By8Bits)
import qualified StructureKit.By8Bits as By8Bits
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
lookup key (By32Bits tree) =
  By6Bits.lookup (KeyOps.toIndexOfLevel1 key) tree
    >>= By6Bits.lookup (KeyOps.toIndexOfLevel2 key)
    >>= By6Bits.lookup (KeyOps.toIndexOfLevel3 key)
    >>= By6Bits.lookup (KeyOps.toIndexOfLevel4 key)
    >>= By8Bits.lookup (KeyOps.toIndexOfLevel5 key)

adjust :: (a -> a) -> Int -> By32Bits a -> By32Bits a
adjust cont key =
  mapCoercible
    ( flip
        By6Bits.adjust
        (KeyOps.toIndexOfLevel1 key)
        ( flip
            By6Bits.adjust
            (KeyOps.toIndexOfLevel2 key)
            ( flip
                By6Bits.adjust
                (KeyOps.toIndexOfLevel3 key)
                ( flip
                    By6Bits.adjust
                    (KeyOps.toIndexOfLevel4 key)
                    ( flip
                        By8Bits.adjust
                        (KeyOps.toIndexOfLevel5 key)
                        cont
                    )
                )
            )
        )
    )

mapAt :: Int -> (a -> a) -> By32Bits a -> By32Bits a
mapAt =
  flip adjust

null :: By32Bits a -> Bool
null (By32Bits tree1) = By6Bits.null tree1

-- * Location API

locate :: Int -> By32Bits a -> Either (Missing a) (Existing a)
locate key (By32Bits tree1) =
  error "TODO"

-- **

data Existing a
  = Existing
      !(By6Bits.Existing (By6Bits (By6Bits (By6Bits (By8Bits a)))))
      !(By6Bits.Existing (By6Bits (By6Bits (By8Bits a))))
      !(By6Bits.Existing (By6Bits (By8Bits a)))
      !(By6Bits.Existing (By8Bits a))
      !(By8Bits.Existing a)

read :: Existing a -> a
read =
  error "TODO"

remove :: Existing a -> By32Bits a
remove =
  error "TODO"

overwrite :: a -> Existing a -> By32Bits a
overwrite val =
  error "TODO"

-- **

data Missing a
  = At1Missing
      !(By6Bits.Missing (By6Bits (By6Bits (By6Bits (By8Bits a)))))
  | At2Missing
      !(By6Bits.Existing (By6Bits (By6Bits (By6Bits (By8Bits a)))))
      !(By6Bits.Missing (By6Bits (By6Bits (By8Bits a))))
  | At3Missing
      !(By6Bits.Existing (By6Bits (By6Bits (By6Bits (By8Bits a)))))
      !(By6Bits.Existing (By6Bits (By6Bits (By8Bits a))))
      !(By6Bits.Missing (By6Bits (By8Bits a)))
  | At4Missing
      !(By6Bits.Existing (By6Bits (By6Bits (By6Bits (By8Bits a)))))
      !(By6Bits.Existing (By6Bits (By6Bits (By8Bits a))))
      !(By6Bits.Existing (By6Bits (By8Bits a)))
      !(By6Bits.Missing (By8Bits a))
  | At5Missing
      !(By6Bits.Existing (By6Bits (By6Bits (By6Bits (By8Bits a)))))
      !(By6Bits.Existing (By6Bits (By6Bits (By8Bits a))))
      !(By6Bits.Existing (By6Bits (By8Bits a)))
      !(By6Bits.Existing (By8Bits a))
      !(By8Bits.Missing a)

write :: a -> Missing a -> By32Bits a
write val =
  error "TODO"

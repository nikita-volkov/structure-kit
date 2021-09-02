-- |
-- Map indexed by 32 bits.
module StructureKit.By32Bits
  ( By32Bits,
    empty,
    lookup,
    adjust,
    mapAt,
  )
where

import qualified StructureKit.By32Bits.KeyOps as KeyOps
import StructureKit.By6Bits (By6Bits)
import qualified StructureKit.By6Bits as By6Bits
import StructureKit.By8Bits (By8Bits)
import qualified StructureKit.By8Bits as By8Bits
import StructureKit.Prelude hiding (adjust, empty, lookup)

newtype By32Bits a
  = By32Bits (By6Bits (By6Bits (By6Bits (By6Bits (By8Bits a)))))

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

-- |
-- Map indexed by 32 bits.
module StructureKit.By32Bits
  ( -- *
    By32Bits,
    empty,
    lookup,
    adjust,
    mapAt,

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

import qualified StructureKit.By32Bits.KeyOps as KeyOps
import StructureKit.By6Bits (By6Bits)
import qualified StructureKit.By6Bits as By6Bits
import StructureKit.By8Bits (By8Bits)
import qualified StructureKit.By8Bits as By8Bits
import StructureKit.Prelude hiding (adjust, empty, lookup, read, remove, write)

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

-- * Selection API

select :: Int -> By32Bits a -> Either (Missing a) (Present a)
select key (By32Bits tree1) =
  case By6Bits.select key1 tree1 of
    Right present1 ->
      let remove1 = By32Bits $ By6Bits.remove present1
          overwrite1 val = By32Bits $ By6Bits.overwrite val present1
       in case By6Bits.select key2 (By6Bits.read present1) of
            Right present2 ->
              let remove2 = levelRemove remove1 overwrite1 present2
                  overwrite2 = levelOverwrite overwrite1 present2
               in case By6Bits.select key3 (By6Bits.read present2) of
                    Right present3 ->
                      let remove3 = levelRemove remove2 overwrite2 present3
                          overwrite3 = levelOverwrite overwrite2 present3
                       in case By6Bits.select key4 (By6Bits.read present3) of
                            Right present4 ->
                              let remove4 = levelRemove remove3 overwrite3 present4
                                  overwrite4 = levelOverwrite overwrite3 present4
                               in case By8Bits.select key5 (By6Bits.read present4) of
                                    Right present5 ->
                                      Right $
                                        Present
                                          (By8Bits.read present5)
                                          ( let tree5 = By8Bits.remove present5
                                             in if By8Bits.null tree5
                                                  then remove4
                                                  else overwrite4 tree5
                                          )
                                          (\val -> overwrite4 $ By8Bits.overwrite val present5)
                                    Left missing5 ->
                                      Left $
                                        Missing
                                          (\val -> overwrite4 $ By8Bits.write val missing5)
                            Left missing4 ->
                              Left $
                                Missing
                                  ( \val ->
                                      missing4
                                        & By6Bits.write (By8Bits.singleton key5 val)
                                        & overwrite3
                                  )
                    Left missing3 ->
                      Left $
                        Missing
                          ( \val ->
                              let tree4 =
                                    By6Bits.singleton key4 $ By8Bits.singleton key5 val
                               in missing3 & By6Bits.write tree4 & overwrite2
                          )
            Left missing2 ->
              Left $
                Missing
                  ( \val ->
                      let tree3 =
                            By6Bits.singleton key3 $ By6Bits.singleton key4 $ By8Bits.singleton key5 val
                       in missing2 & By6Bits.write tree3 & overwrite1
                  )
    Left missing1 ->
      Left $
        Missing
          ( \val ->
              let tree2 =
                    By6Bits.singleton key2 $ By6Bits.singleton key3 $ By6Bits.singleton key4 $ By8Bits.singleton key5 val
               in missing1 & By6Bits.write tree2 & By32Bits
          )
  where
    key1 = KeyOps.toIndexOfLevel1 key
    key2 = KeyOps.toIndexOfLevel2 key
    key3 = KeyOps.toIndexOfLevel3 key
    key4 = KeyOps.toIndexOfLevel4 key
    key5 = KeyOps.toIndexOfLevel5 key
    levelRemove removeFromParent overwriteInParent present =
      By6Bits.remove present & \tree ->
        if By6Bits.null tree
          then removeFromParent
          else overwriteInParent tree
    levelOverwrite overwriteInParent present val =
      overwriteInParent $ By6Bits.overwrite val present

-- **

data Present a
  = Present ~a ~(By32Bits a) (a -> By32Bits a)

read :: Present a -> a
read (Present x _ _) = x

remove :: Present a -> By32Bits a
remove (Present _ x _) = x

overwrite :: a -> Present a -> By32Bits a
overwrite val (Present _ _ x) = x val

-- **

newtype Missing a
  = Missing (a -> By32Bits a)

write :: a -> Missing a -> By32Bits a
write val (Missing x) = x val

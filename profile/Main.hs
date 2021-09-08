import qualified StructureKit.LruHashCache as LruHashCache
import qualified StructureKit.QuickCheckUtil.ExtrasFor.Gen as GenExtras
import qualified Test.QuickCheck.Gen as Gen
import Prelude

main =
  uncurry performLookups generate

{-# NOINLINE performLookups #-}
performLookups :: (Hashable k, Eq k) => LruHashCache.LruHashCache k v -> [k] -> IO ()
performLookups lhc existingKeys =
  do
    res <-
      {-# SCC "performLookups" #-}
      existingKeys
        & replicate 1000
        & concat
        & fmap (`LruHashCache.lookup` lhc)
        & evaluate
    print $ length $ filter (isJust . fst) $ res

{-# NOINLINE generate #-}
generate =
  {-# SCC "generate" #-}
  GenExtras.seedGen gen 0
  where
    gen = do
      inserts <- replicateM 1000 $ do
        key <- GenExtras.sentence
        val <- Gen.chooseInt64 (0, 9)
        return (key, val)
      let !lhc = LruHashCache.empty 500 & LruHashCache.insertMany inserts & snd
          !existingKeys = inserts & drop 500 & fmap fst
      return $!! (lhc, existingKeys)

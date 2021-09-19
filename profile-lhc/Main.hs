import qualified StructureKit.LruHashCache as LruHashCache
import qualified StructureKit.QuickCheckUtil.ExtrasFor.Gen as GenExtras
import qualified Test.QuickCheck.Gen as Gen
import Prelude

main =
  uncurry performLookups generate

{-# SCC performLookups #-}
{-# NOINLINE performLookups #-}
performLookups :: (Hashable k, Eq k) => LruHashCache.LruHashCache k Int64 -> [k] -> IO ()
performLookups !lhc !existingKeys =
  existingKeys
    & replicate 1000
    & concat
    & foldl'
      (\(!x, !lhc) k -> LruHashCache.lookup k lhc & maybe (x, lhc) (first (+ x)))
      (0, lhc)
    & \(!x, !lhc) -> print x

{-# SCC generate #-}
{-# NOINLINE generate #-}
generate =
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

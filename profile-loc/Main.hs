import StructureKit.LruOrdCache qualified as LruOrdCache
import StructureKit.QuickCheckUtil.ExtrasFor.Gen qualified as GenExtras
import Test.QuickCheck.Gen qualified as Gen
import Prelude

main =
  uncurry performLookups generate

{-# SCC performLookups #-}
{-# NOINLINE performLookups #-}
performLookups :: (Ord k) => LruOrdCache.LruOrdCache k Int64 -> [k] -> IO ()
performLookups !lhc !existingKeys =
  existingKeys
    & replicate 1000
    & concat
    & foldl'
      (\(!x, !lhc) k -> LruOrdCache.lookup k lhc & \(eviction, lhc) -> (maybe x (+ x) eviction, lhc))
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
      let !lhc = LruOrdCache.empty 500 & LruOrdCache.insertMany inserts & snd
          !existingKeys = inserts & drop 500 & fmap fst
      return $!! (lhc, existingKeys)

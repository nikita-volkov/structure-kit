import Gauge.Main
import qualified Main.ExtrasFor.LruHashCache as LruHashCacheExtras
import qualified Main.ExtrasFor.QcGen as GenExtras
import qualified StructureKit.LruHashCache as LruHashCache
import qualified Test.QuickCheck.Gen as Gen
import qualified Test.QuickCheck.Random as Random
import Prelude

main =
  defaultMain
    [ bgroup "lru-hash-cache" lruHashCache
    ]

lruHashCache =
  GenExtras.seedGen gen 0
  where
    gen = do
      inserts <- replicateM 1000 $ do
        key <- GenExtras.sentence
        val <- Gen.chooseInt64 (0, 9)
        return (key, val)
      let !lhc = snd $ LruHashCacheExtras.insertMany inserts $ LruHashCache.empty 500
          !existingKey = fst $ inserts !! 777
      case LruHashCache.lookup existingKey lhc of
        (Nothing, _) -> error "Key not found"
        _ -> return ()
      return $
        [ bench "lookup" $ nf (LruHashCache.lookup existingKey) lhc
        ]

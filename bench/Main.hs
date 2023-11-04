module Main where

import Criterion.Main
import Data.HashMap.Strict qualified as HashMap
import Data.Map.Strict qualified as Map
import StructureKit.LruHashCache qualified as LruHashCache
import StructureKit.LruOrdCache qualified as LruOrdCache
import StructureKit.QuickCheckUtil.ExtrasFor.Gen qualified as GenExtras
import Test.QuickCheck.Gen qualified as Gen
import Test.QuickCheck.Random qualified as Random
import Prelude

main =
  defaultMain $ GenExtras.seedGen gen 0
  where
    gen = do
      inserts <- replicateM 1000 $ do
        key <- GenExtras.sentence
        val <- Gen.chooseInt64 (0, 9)
        return (key, val)
      let !lhc = snd $ LruHashCache.insertMany inserts $ LruHashCache.empty 500
          !loc = snd $ LruOrdCache.insertMany inserts $ LruOrdCache.empty 500
          !existingKey = fst $ inserts !! 777
          !hashMap = HashMap.fromList inserts
          !map = Map.fromList inserts
      case LruHashCache.lookup existingKey lhc of
        Nothing -> error "Key not found"
        _ -> return ()
      return
        $ [ bgroup
              "lru-ord-cache"
              [ bench "lookup" $ nf (LruOrdCache.lookup existingKey) loc,
                bench "insert-missing" $ nf (LruOrdCache.insert "something new" 7) loc,
                bench "insert-existing" $ nf (LruOrdCache.insert existingKey 7) loc
              ],
            bgroup
              "lru-hash-cache"
              [ bench "lookup" $ nf (LruHashCache.lookup existingKey) lhc,
                bench "insert-missing" $ nf (LruHashCache.insert "something new" 7) lhc,
                bench "insert-existing" $ nf (LruHashCache.insert existingKey 7) lhc
              ],
            bgroup
              "std-hash-map"
              [ bench "lookup" $ nf (HashMap.lookup existingKey) hashMap,
                bench "insert-missing" $ nf (HashMap.insert "something new" 7) hashMap,
                bench "insert-existing" $ nf (HashMap.insert existingKey 7) hashMap
              ],
            bgroup
              "std-map"
              [ bench "lookup" $ nf (Map.lookup existingKey) map,
                bench "insert-missing" $ nf (Map.insert "something new" 7) map,
                bench "insert-existing" $ nf (Map.insert existingKey 7) map
              ]
          ]

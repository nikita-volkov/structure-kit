module Main where

import qualified Data.List as List
import qualified StructureKit.Bits64 as Bits64
import qualified StructureKit.By32Bits as By32Bits
import qualified StructureKit.By6Bits as By6Bits
import qualified StructureKit.LruHashCache as LruHashCache
import qualified Test.QuickCheck as QuickCheck
import Test.QuickCheck.Instances
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.Tasty.Runners
import Prelude hiding (assert)

main =
  defaultMain . testGroup "All tests" $
    [ testGroup "Bits64" bits64,
      testGroup "By6Bits" by6Bits,
      testGroup "By32Bits" by32Bits,
      testGroup "LruHashCache" lruHashCache
    ]

by6Bits =
  [testGroup "old" old, testGroup "new" new]
  where
    old =
      [ testProperty "Inserted value can be looked up" $ do
          insertionList <- listOf $ do
            k <- chooseInt (0, 63)
            v <- arbitrary @Word8
            return (k, v)
          return $
            let nubbedInsertionList =
                  nubBy (on (==) fst) insertionList
                map =
                  foldl' (\map (k, v) -> snd (By6Bits.insert k v map)) By6Bits.empty nubbedInsertionList
                lookupList =
                  fmap (\(k, _) -> (k, By6Bits.lookup k map)) nubbedInsertionList
                expectedLookupList =
                  fmap (\(k, v) -> (k, Just v)) nubbedInsertionList
             in lookupList === expectedLookupList
      ]
    new =
      [ testProperty "Inserted entry is accessible" $ do
          map <- genMap
          key <- chooseInt (0, 63)
          val <- arbitrary
          let map' = insert key val map
          return . counterexamples [show map, show map', show key] $
            Just val === lookup key map'
      ]
      where
        genMap = do
          size <- chooseInt (0, 63)
          inserts <- replicateM size ((,) <$> chooseInt (0, 63) <*> arbitrary @Word16)
          return $ foldr (uncurry insert) By6Bits.empty inserts
        insert key val map =
          By6Bits.locate key map
            & either (By6Bits.write val) (By6Bits.overwrite val)
        lookup key map =
          By6Bits.locate key map
            & either (const Nothing) (Just . By6Bits.read)

bits64 =
  [ testProperty "Inserted entry is accessible" $ do
      x <- genBits64
      key <- chooseInt (0, 63)
      let x' = insert key x
      return . counterexample (show x) $
        lookup key x',
    testProperty "Removed entry is inaccessible" $ do
      x <- genBits64
      key <- chooseInt (0, 63)
      let x' = delete key x
      return . counterexample (show x) . counterexample (show x') . counterexample (show key) $
        not $ lookup key x'
  ]
  where
    genBits64 =
      unsafeCoerce @Word64 @Bits64.Bits64 <$> arbitrary
    insert key x =
      case Bits64.locate key x of
        Bits64.FoundLocation _ _ -> x
        Bits64.UnfoundLocation _ x -> x
    delete key x =
      case Bits64.locate key x of
        Bits64.FoundLocation _ x -> x
        Bits64.UnfoundLocation _ _ -> x
    lookup key x =
      Bits64.locate key x & \case
        Bits64.FoundLocation _ _ -> True
        _ -> False

by32Bits =
  [ testProperty "Inserted entry is accessible" $ do
      map <- genMap
      key <- arbitrary
      val <- arbitrary
      let map' = insert key val map
      return $
        Just val === lookup key map'
  ]
  where
    genMap = do
      size <- chooseInt (0, 999)
      inserts <- replicateM size (arbitrary @(Int, Word16))
      return $ foldr (uncurry insert) By32Bits.empty inserts
    insert key val map =
      By32Bits.locate key map
        & either (By32Bits.write val) (By32Bits.overwrite val)
    lookup key map =
      By32Bits.locate key map
        & either (const Nothing) (Just . By32Bits.read)

lruHashCache =
  [ testProperty "Freshly inserted entry must be possible to lookup" $ do
      initialSize <- chooseInt (0, 999)
      cap <- chooseInt (1, 999)
      inserts <- replicateM initialSize (arbitrary @(Word16, Word16))
      let lhc = fromInserts cap inserts
      key <- arbitrary
      val <- arbitrary
      let lhc' = LruHashCache.insert key val lhc & snd
          lookupRes = LruHashCache.lookup key lhc' & fst
      return $
        Just val === lookupRes,
    testProperty "Records get evicted in the order of last lookup" $ do
      size <- chooseInt (0, 999)
      initialEntries <- forM [1 .. size] $ \k -> (k,) <$> arbitrary @Word8
      otherEntries <- forM [1001 .. (1000 + size)] $ \k -> (k,) <$> arbitrary
      keysToLookup <- shuffle (fmap fst initialEntries)
      let initialLhc =
            fromInserts size initialEntries
          lhcAfterLookups =
            foldl' (\lhc k -> snd (LruHashCache.lookup k lhc)) initialLhc keysToLookup
          evictions =
            lhcAfterLookups & LruHashCache.insertMany otherEntries & fst & fmap fst & reverse
      return $
        keysToLookup === evictions,
    testProperty "Inserting new entry after cap is reached produces evicted entry" $ do
      size <- chooseInt (0, 99)
      let keys = enumFromTo 1 size
      inserts <- forM keys $ \k -> (k,) <$> arbitrary @Word8
      val <- arbitrary
      let lhc = fromInserts size inserts
          inexistentKey = size + 1
          eviction = LruHashCache.insert inexistentKey val lhc & fst
      return $ isJust eviction,
    testProperty "Evicted entry cannot be looked up" $ do
      size <- chooseInt (0, 99)
      initialEntries <- replicateM size $ arbitrary @(Word16, Word16)
      let lhc = fromInserts size initialEntries
      key <- arbitrary
      val <- arbitrary
      case LruHashCache.insert key val lhc of
        (eviction, lhc) -> case eviction of
          Nothing -> discard
          Just (key, val) ->
            let lookupRes = fst $ LruHashCache.lookup key lhc
             in return $ Nothing === lookupRes
  ]
  where
    fromInserts cap list =
      LruHashCache.empty cap & LruHashCache.insertMany list & snd

counterexamples =
  foldr (.) id . fmap counterexample

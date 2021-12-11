module Main where

import qualified Data.List as List
import qualified StructureKit.Bits64 as Bits64
import qualified StructureKit.By32Bits as By32Bits
import qualified StructureKit.By6Bits as By6Bits
import qualified StructureKit.LruHashCache as LruHashCache
import qualified StructureKit.LruOrdCache as LruOrdCache
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
      testGroup "LruHashCache" lruHashCache,
      testGroup "LruOrdCache" lruOrdCache
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
  [ testProperty "toList produces the insertion list" $ do
      inserts <- listOf (arbitrary @(Word16, Word16))
      let nubbedInserts =
            inserts
              & reverse
              & nubBy (on (==) fst)
              & reverse
          nubbedSize =
            length nubbedInserts
          maxCap =
            succ nubbedSize * 2
      cap <- chooseInt (1, maxCap)
      let recordsSupposedToBeEvicted =
            if cap > nubbedSize then 0 else nubbedSize - cap
          insertsSupposedToBePresent =
            nubbedInserts
              & drop recordsSupposedToBeEvicted
          cache =
            LruHashCache.empty cap
              & LruHashCache.insertMany inserts
              & snd
      return $ insertsSupposedToBePresent === LruHashCache.toList cache,
    testProperty "Running insertMany results in the same as running insert multiple times" $ do
      inserts <- listOf (arbitrary @(Word16, Word16))
      let nubbedInserts =
            inserts
              & reverse
              & nubBy (on (==) fst)
              & reverse
          nubbedSize =
            length nubbedInserts
      cap <- chooseInt (1, nubbedSize)
      let initialCache = LruHashCache.empty cap
          usingMultipleInserts =
            foldl'
              ( \(!evictions, !lhc) (k, v) ->
                  LruHashCache.insert k v lhc & \(eviction, lhc) ->
                    let newEvictions = case eviction of
                          Just eviction -> eviction : evictions
                          Nothing -> evictions
                     in (newEvictions, lhc)
              )
              ([], initialCache)
              inserts
          usingInsertMany =
            LruHashCache.insertMany inserts initialCache
      return . counterexample ("Size: " <> show nubbedSize <> "; Cap: " <> show cap) $
        label "Entries equal" (on (===) (LruHashCache.toList . snd) usingMultipleInserts usingInsertMany)
          .&&. label "Evictions equal" (fst usingMultipleInserts === fst usingInsertMany),
    testProperty "Freshly inserted entry must be possible to lookup" $ do
      initialSize <- chooseInt (0, 999)
      cap <- chooseInt (1, 999)
      inserts <- replicateM initialSize (arbitrary @(Word16, Word16))
      let lhc = fromInserts cap inserts
      key <- arbitrary
      val <- arbitrary
      let lhc' = LruHashCache.insert key val lhc & snd
          lookupRes = LruHashCache.lookup key lhc' & fmap fst
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
            foldl' (\lhc k -> LruHashCache.lookup k lhc & maybe lhc snd) initialLhc keysToLookup
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
            let lookupRes = LruHashCache.lookup key lhc & fmap fst
             in return $ Nothing === lookupRes
  ]
  where
    fromInserts cap list =
      LruHashCache.empty cap & LruHashCache.insertMany list & snd

lruOrdCache =
  [ testProperty "toList produces the insertion list" $ do
      inserts <- listOf (arbitrary @(Word16, Word16))
      let nubbedInserts =
            inserts
              & reverse
              & nubBy (on (==) fst)
              & reverse
          nubbedSize =
            length nubbedInserts
          maxCap =
            succ nubbedSize * 2
      cap <- chooseInt (1, maxCap)
      let recordsSupposedToBeEvicted =
            if cap > nubbedSize then 0 else nubbedSize - cap
          insertsSupposedToBePresent =
            nubbedInserts
              & drop recordsSupposedToBeEvicted
          cache =
            LruOrdCache.empty cap
              & LruOrdCache.insertMany inserts
              & snd
      return $ insertsSupposedToBePresent === LruOrdCache.toList cache,
    testProperty "Running insertMany results in the same as running insert multiple times" $ do
      inserts <- listOf (arbitrary @(Word16, Word16))
      let nubbedInserts =
            inserts
              & reverse
              & nubBy (on (==) fst)
              & reverse
          nubbedSize =
            length nubbedInserts
      cap <- chooseInt (1, nubbedSize)
      let initialCache = LruOrdCache.empty cap
          usingMultipleInserts =
            foldl'
              ( \(!evictions, !lhc) (k, v) ->
                  LruOrdCache.insert k v lhc & \(eviction, lhc) ->
                    let newEvictions = case eviction of
                          Just eviction -> eviction : evictions
                          Nothing -> evictions
                     in (newEvictions, lhc)
              )
              ([], initialCache)
              inserts
          usingInsertMany =
            LruOrdCache.insertMany inserts initialCache
      return . counterexample ("Size: " <> show nubbedSize <> "; Cap: " <> show cap) $
        label "Entries equal" (on (===) (LruOrdCache.toList . snd) usingMultipleInserts usingInsertMany)
          .&&. label "Evictions equal" (fst usingMultipleInserts === fst usingInsertMany),
    testProperty "Freshly inserted entry must be possible to lookup" $ do
      initialSize <- chooseInt (0, 999)
      cap <- chooseInt (1, 999)
      inserts <- replicateM initialSize (arbitrary @(Word16, Word16))
      let lhc = fromInserts cap inserts
      key <- arbitrary
      val <- arbitrary
      let lhc' = LruOrdCache.insert key val lhc & snd
          lookupRes = LruOrdCache.lookup key lhc' & fst
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
            foldl' (\lhc k -> LruOrdCache.lookup k lhc & snd) initialLhc keysToLookup
          evictions =
            lhcAfterLookups & LruOrdCache.insertMany otherEntries & fst & fmap fst & reverse
      return $
        keysToLookup === evictions,
    testProperty "Inserting new entry after cap is reached produces evicted entry" $ do
      size <- chooseInt (0, 99)
      let keys = enumFromTo 1 size
      inserts <- forM keys $ \k -> (k,) <$> arbitrary @Word8
      val <- arbitrary
      let lhc = fromInserts size inserts
          inexistentKey = size + 1
          eviction = LruOrdCache.insert inexistentKey val lhc & fst
      return $ isJust eviction,
    testProperty "Evicted entry cannot be looked up" $ do
      size <- chooseInt (0, 99)
      initialEntries <- replicateM size $ arbitrary @(Word16, Word16)
      let lhc = fromInserts size initialEntries
      key <- arbitrary
      val <- arbitrary
      case LruOrdCache.insert key val lhc of
        (eviction, lhc) -> case eviction of
          Nothing -> discard
          Just (key, val) ->
            let lookupRes = LruOrdCache.lookup key lhc & fst
             in return $ Nothing === lookupRes
  ]
  where
    fromInserts cap list =
      LruOrdCache.empty cap & LruOrdCache.insertMany list & snd

counterexamples =
  foldr (.) id . fmap counterexample

module Main where

import qualified Data.List as List
import qualified StructureKit.By6Bits as By6Bits
import qualified Test.QuickCheck as QuickCheck
import Test.QuickCheck.Instances
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.Tasty.Runners
import Prelude hiding (assert)

main =
  defaultMain $
    testGroup
      "All tests"
      [ testGroup
          "By6Bits"
          [ testProperty
              "Inserted value can be looked up"
              do
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
      ]

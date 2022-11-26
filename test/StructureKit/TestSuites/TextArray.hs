module StructureKit.TestSuites.TextArray where

import Data.List.Extra qualified as ExtraList
import StructureKit.TextArray (TextArray)
import StructureKit.TextArray qualified as TextArray
import Test.QuickCheck qualified as QuickCheck
import Test.QuickCheck.Instances
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.Tasty.Runners
import Prelude hiding (assert)

tests =
  [ testProperty "(from/to)List" $ \list ->
      list === TextArray.toList (TextArray.fromList list),
    testProperty "lookup" $ \(list, index :: Word16) ->
      let intIndex = fromIntegral index
       in list ExtraList.!? intIndex
            === TextArray.lookup intIndex (TextArray.fromList list)
  ]

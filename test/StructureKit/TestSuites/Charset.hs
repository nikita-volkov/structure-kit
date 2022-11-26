module StructureKit.TestSuites.Charset where

import Data.String.ToString as Exports (ToString (..))
import StructureKit.Charset (Charset)
import StructureKit.Charset qualified as Charset
import Test.QuickCheck qualified as QuickCheck
import Test.QuickCheck.Instances
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.Tasty.Runners
import Prelude hiding (assert)

tests =
  [ testProperty "(from/to)String" $ \string ->
      let reference = nub (sort string)
       in reference === toList (fromString @Charset string)
  ]

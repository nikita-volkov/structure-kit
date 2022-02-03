module StructureKit.TestSuites.Charset where

import Data.String.ToString as Exports (ToString (..))
import StructureKit.Charset (Charset)
import qualified StructureKit.Charset as Charset
import qualified Test.QuickCheck as QuickCheck
import Test.QuickCheck.Instances
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.Tasty.Runners
import Prelude hiding (assert)

tests =
  [ testProperty "(from/to)String" $ \string ->
      let reference = nub (sort string)
       in reference === toString (fromString @Charset string)
  ]

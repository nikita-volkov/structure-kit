module StructureKit.Charset
  ( -- *
    Charset,

    -- *
    contains,

    -- *
    range,
  )
where

import qualified Data.RangeSet.IntMap as IntRange
import StructureKit.Prelude hiding (range)

-- *

newtype Charset
  = Charset IntRange.RIntSet
  deriving (Eq, Ord, Show, Semigroup, Monoid, NFData)

-- *

contains :: Char -> Charset -> Bool
contains char (Charset intRange) =
  IntRange.member (ord char) intRange

-- *

range :: Char -> Char -> Charset
range = error "TODO"

text :: Text -> Charset
text = error "TODO"

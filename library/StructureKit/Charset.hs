module StructureKit.Charset
  ( -- *
    Charset,

    -- *
    contains,

    -- *
    charRange,
    codepointRange,
    string,
    text,
  )
where

import qualified Data.RangeSet.IntMap as IntRange
import StructureKit.Prelude hiding (range)

-- *

newtype Charset
  = Charset IntRange.RIntSet
  deriving (Eq, Ord, Show, Semigroup, Monoid, NFData)

instance IsString Charset where
  fromString = string

instance ToString Charset where
  toString = fmap chr . IntRange.toAscList . coerce

instance Hashable Charset where
  hashWithSalt salt (Charset range) =
    hashWithSalt salt (IntRange.toAscList range)

-- *

-- |
-- Convert a charset to predicate.
contains :: Charset -> Char -> Bool
contains (Charset intRange) char =
  IntRange.member (ord char) intRange

-- *

charRange :: Char -> Char -> Charset
charRange min max =
  Charset $ IntRange.singletonRange (ord min, ord max)

codepointRange :: Int -> Int -> Charset
codepointRange min max =
  Charset $ IntRange.singletonRange (min, max)

codepointList :: [Int] -> Charset
codepointList = Charset . IntRange.fromList

string :: String -> Charset
string = codepointList . fmap ord

text :: Text -> Charset
text = string . toString

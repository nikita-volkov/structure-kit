module StructureKit.Charset
  ( -- * --
    Charset,

    -- * Execution
    toCharPredicate,
    toCodepointPredicate,
    findInText,

    -- * Construction
    charRange,
    codepointRange,
    string,
    text,

    -- * Specific
    upperLatin,
    lowerLatin,
    latin,
    num,
  )
where

import qualified Data.RangeSet.IntMap as IntRange
import qualified Data.Text as Text
import StructureKit.Prelude hiding (range)

-- * --

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

-- * --

-- |
-- Convert a charset to a predicate that checks whether it contains a char.
toCharPredicate :: Charset -> Char -> Bool
toCharPredicate (Charset intRange) char =
  IntRange.member (ord char) intRange

toCodepointPredicate :: Charset -> Int -> Bool
toCodepointPredicate (Charset intRange) codepoint =
  IntRange.member codepoint intRange

findInText :: Charset -> Text -> Maybe Char
findInText charset =
  Text.find (toCharPredicate charset)

-- * --

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

-- * Specific

upperLatin :: Charset
upperLatin = charRange 'A' 'Z'

lowerLatin :: Charset
lowerLatin = charRange 'a' 'z'

-- | Latin chars.
latin :: Charset
latin = upperLatin <> lowerLatin

-- | Numerals.
num :: Charset
num = charRange '0' '9'

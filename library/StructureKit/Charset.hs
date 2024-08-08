module StructureKit.Charset
  ( -- * --
    Charset,

    -- * Execution
    toCharPredicate,
    toCodepointPredicate,
    toCharRangeList,
    toCodepointRangeList,
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
    hexDigit,
  )
where

import Data.RangeSet.IntMap qualified as IntRange
import Data.Text qualified as Text
import StructureKit.Prelude hiding (range)

-- * --

newtype Charset
  = Charset IntRange.RIntSet
  deriving (Eq, Ord, Show, Semigroup, Monoid, NFData)

instance IsString Charset where
  fromString = string

instance IsList Charset where
  type Item Charset = Char
  fromList = string
  toList = fmap chr . IntRange.toAscList . coerce

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

toCharRangeList :: Charset -> [(Char, Char)]
toCharRangeList =
  fmap (\(a, b) -> (chr a, chr b)) . toCodepointRangeList

toCodepointRangeList :: Charset -> [(Int, Int)]
toCodepointRangeList (Charset intRange) =
  IntRange.toRangeList intRange

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
text = string . toList

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

-- | Hexadecimal digits.
{-# NOINLINE hexDigit #-}
hexDigit :: Charset
hexDigit = charRange '0' '9' <> charRange 'a' 'f' <> charRange 'A' 'F'

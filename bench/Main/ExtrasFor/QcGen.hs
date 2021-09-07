module Main.ExtrasFor.QcGen where

import qualified Data.Text as Text
import qualified Main.ExtrasFor.LruHashCache as LruHashCacheExtras
import qualified StructureKit.LruHashCache as LruHashCache
import Test.QuickCheck.Gen
import qualified Test.QuickCheck.Random as Random
import Prelude hiding (choose, optional)

-- * Execution

seedGen :: Gen a -> Int -> a
seedGen gen seed = unGen gen (Random.mkQCGen 0) seed

-- * General

maybeOf :: Gen a -> Gen (Maybe a)
maybeOf gen =
  oneof [pure Nothing, Just <$> gen]

onceIn :: Int -> Gen Bool
onceIn n =
  (== 1) <$> chooseInt (1, n)

-- * Text

text :: Gen Text
text =
  do
    firstSentence <- sentence
    otherSentences <- listOf $ do
      doParagraph <- onceIn 5
      let prefix = if doParagraph then "\n" else " "
      (prefix <>) <$> sentence
    return (mconcat (firstSentence : otherSentences))

sentence :: Gen Text
sentence =
  do
    firstWord <- Text.toTitle <$> word
    extraWordsAmount <- chooseInt (0, 20)
    extraWords <- replicateM extraWordsAmount $ do
      prefix <- do
        prependPunctuation <- (== 0) <$> chooseInt (0, 9)
        if prependPunctuation
          then elements [", ", ": ", " - "]
          else pure " "
      theWord <- do
        titleCase <- (== 0) <$> chooseInt (0, 9)
        (if titleCase then Text.toTitle else id) <$> word
      return (prefix <> theWord)
    return (firstWord <> mconcat extraWords)

word :: Gen Text
word =
  elements
    ["foo", "bar", "qux", "quux", "quuz", "corge", "grault", "garply", "waldo", "fred", "plugh", "xyzzy", "thud"]

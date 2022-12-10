module StructureKit.TextArray
  ( TextArray,
    fromList,
    toList,
    lookup,
  )
where

import Data.Text.Array qualified as TextArray
import Data.Text.Internal qualified as TextInternal
import Data.Vector.Unboxed qualified as UVec
import GHC.Base qualified as GhcBase
import StructureKit.Prelude hiding (fromList, lookup, toList)
import Text.Builder qualified as TextBuilder
import VectorExtras.Accumulator qualified as VAcc
import VectorExtras.Generic qualified

-- |
-- A space-efficient representation of an array of text chunks.
data TextArray = TextArray
  { flatArray :: !TextArray.Array,
    bounds :: !(UVec (Int, Int))
  }

sliceText :: Text -> [Int] -> TextArray
sliceText (TextInternal.Text array offset length) bounds =
  error "TODO" $
    foldr stepBounds finishBounds bounds offset VAcc.init
  where
    stepBounds bound next !prevOffset !boundsAcc =
      if prevOffset < bound
        then next bound $ VAcc.add (prevOffset, bound) boundsAcc
        else next bound $ boundsAcc
    finishBounds prevOffset boundsAcc =
      error "TODO"

fromTextBuilderList :: [TextBuilder.Builder] -> TextArray
fromTextBuilderList =
  error "TODO"

fromList :: [Text] -> TextArray
fromList textList =
  foldr step finish textList 0 0 []
  where
    step (TextInternal.Text _ offset length) next !totalLength !totalCount !boundRevList =
      next (totalLength + length) (succ totalCount) ((totalLength, length) : boundRevList)
    finish totalLength totalCount boundRevList =
      TextArray flatArray boundVec
      where
        boundVec =
          VectorExtras.Generic.fromReverseListN totalCount boundRevList
        flatArray =
          TextArray.run $ do
            dstArray <- TextArray.new totalLength
            foldM (foldStep dstArray) 0 textList
            return dstArray
          where
            foldStep dstArray dstOffset (TextInternal.Text srcArray srcOffset length) =
              TextArray.copyI length dstArray dstOffset srcArray srcOffset
                $> dstOffset + length

{-# INLINE toList #-}
toList :: TextArray -> [Text]
toList TextArray {..} =
  GhcBase.build builder
  where
    builder :: (Text -> b -> b) -> b -> b
    builder cons nil =
      UVec.foldr step nil bounds
      where
        step (offset, length) =
          cons
            (TextInternal.Text flatArray offset length)

{-# INLINE lookup #-}
lookup :: Int -> TextArray -> Maybe Text
lookup index TextArray {..} =
  bounds UVec.!? index <&> \(offset, length) ->
    TextInternal.Text flatArray offset length

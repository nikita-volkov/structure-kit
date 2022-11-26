module StructureKit.TextArray
  ( TextArray,
    fromList,
    toList,
  )
where

import Data.Text.Array qualified as TextArray
import Data.Text.Internal qualified as TextInternal
import Data.Vector.Unboxed qualified as UVec
import GHC.Base qualified as GhcBase
import StructureKit.Prelude hiding (fromList, toList)
import VectorExtras.Generic qualified

-- |
-- A space-efficient representation of an array of text chunks.
data TextArray = TextArray
  { -- | Array.
    flatArray :: !TextArray.Array,
    -- | Lengths.
    lengths :: !(UVec Int)
  }

fromList :: [Text] -> TextArray
fromList textList =
  foldr step finish textList 0 0 []
  where
    step (TextInternal.Text _ offset length) next !totalLength !totalCount !lengthRevList =
      next (totalLength + length) (succ totalCount) (length : lengthRevList)
    finish totalLength totalCount lengthRevList =
      TextArray flatArray lengthVec
      where
        lengthVec =
          VectorExtras.Generic.fromReverseListN totalCount lengthRevList
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
      UVec.foldr step finish lengths 0
      where
        step length next !offset =
          cons
            (TextInternal.Text flatArray offset length)
            (next (offset + length))
        finish _ =
          nil

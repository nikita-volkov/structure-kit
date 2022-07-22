module StructureKit.SegmentMap where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import StructureKit.Prelude
import StructureKit.Range (Range)
import qualified StructureKit.Range as Range

newtype SegmentMap k v = SegmentMap
  { rangeIndexedMap :: Map k (Entry k v)
  }

data Entry k v = Entry
  { entryEndKey :: k,
    entryValue :: v
  }

lookup :: Ord k => k -> SegmentMap k v -> Maybe (Range k, v)
lookup k SegmentMap {..} =
  case Map.lookupLE k rangeIndexedMap of
    Just (k, Entry {..}) ->
      Just (Range.Range k entryEndKey, entryValue)
    Nothing ->
      Nothing

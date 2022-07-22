module StructureKit.RangeIndexed where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import StructureKit.Prelude
import StructureKit.Range (Range)
import qualified StructureKit.Range as Range

newtype RangeIndexed k v = RangeIndexed
  { rangeIndexedMap :: Map k (Entry k v)
  }

data Entry k v = Entry
  { entryEndKey :: k,
    entryValue :: v
  }

lookup :: Ord k => k -> RangeIndexed k v -> Maybe (Range k, v)
lookup k RangeIndexed {..} =
  case Map.lookupLE k rangeIndexedMap of
    Just (k, Entry {..}) ->
      Just (Range.Range k entryEndKey, entryValue)
    Nothing ->
      Nothing

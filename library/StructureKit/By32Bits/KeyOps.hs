module StructureKit.By32Bits.KeyOps where

import StructureKit.Prelude

toIndexOfLevel1 a = a .&. 0b111111

toIndexOfLevel2 a = unsafeShiftR a 6 .&. 0b111111

toIndexOfLevel3 a = unsafeShiftR a 12 .&. 0b111111

toIndexOfLevel4 a = unsafeShiftR a 18 .&. 0b111111

toIndexOfLevel5 a = unsafeShiftR a 24 .&. 0b11111111

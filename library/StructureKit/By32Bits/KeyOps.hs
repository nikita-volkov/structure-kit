module StructureKit.By32Bits.KeyOps where

import StructureKit.Prelude

{-# INLINE toIndexOfLevel1 #-}
toIndexOfLevel1 a = a .&. 0b111111

{-# INLINE toIndexOfLevel2 #-}
toIndexOfLevel2 a = unsafeShiftR a 6 .&. 0b111111

{-# INLINE toIndexOfLevel3 #-}
toIndexOfLevel3 a = unsafeShiftR a 12 .&. 0b111111

{-# INLINE toIndexOfLevel4 #-}
toIndexOfLevel4 a = unsafeShiftR a 18 .&. 0b111111

{-# INLINE toIndexOfLevel5 #-}
toIndexOfLevel5 a = unsafeShiftR a 24 .&. 0b11111111

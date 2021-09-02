module StructureKit.TrieBitMasks where

import StructureKit.Prelude

level1 :: Int -> Int
level1 a = a .&. 0b111111

level2 :: Int -> Int
level2 a = unsafeShiftR a 6 .&. 0b111111

level3 :: Int -> Int
level3 a = unsafeShiftR a 12 .&. 0b111111

level4 :: Int -> Int
level4 a = unsafeShiftR a 18 .&. 0b111111

level5 :: Int -> Int
level5 a = unsafeShiftR a 24 .&. 0b111111

module Emulator.Operations.RegRegDes where

import Types

regRegDes :: (Int32 -> Int32 -> Int32) -> Operation
regRegDes op proc arg = do
  let reg0       = unsigned 4 $ arg `shiftR` 23
      reg1       = unsigned 4 $ arg `shiftR` 19
      des        = unsigned 4 $ arg `shiftR` 15
      reg0mode   = unsigned 1 $ arg `shiftR` 14
      reg1mode   = unsigned 1 $ arg `shiftR` 13
      desmode    = unsigned 1 $ arg `shiftR` 12
      reg0offset = unsigned 4 $ arg `shiftR` 8
      reg1offset = unsigned 4 $ arg `shiftR` 4
      desoffset  = unsigned 4 arg
  reg0' <- getReg reg0 reg0mode reg0offset
  reg1' <- getReg reg1 reg1mode reg1offset
  des'  <- getReg des  desmode  desoffset
  writeIOReg des' `fmap` liftM2 op (readIORef reg0') (readIORef reg1')
  return Continue

regRegDesF :: (Int32 -> Int32 -> Int32) -> Operation
regRegDesF op = regRegDes \i0 i1 -> unsafeCoerce $ (unsafeCoerce i0 :: Float) `op` (unsafeCoerce i1 :: Float)

add :: Operation
add = regRegDes (+)

addf :: Operation
addf = regRegDesF (+)

module Emulator.Operations.RegRegDes where

import Types
import Ubi
import Util (getReg)

regRegDes :: (Int32 -> Int32 -> Int32) -> Operation
regRegDes op proc ram arg = do
  let reg0       = unsigned 4 $ arg `shiftR` 23
      reg1       = unsigned 4 $ arg `shiftR` 19
      des        = unsigned 4 $ arg `shiftR` 15
      reg0mode   = unsigned 1 $ arg `shiftR` 14
      reg1mode   = unsigned 1 $ arg `shiftR` 13
      desmode    = unsigned 1 $ arg `shiftR` 12
      reg0offset = unsigned 4 $ arg `shiftR` 8
      reg1offset = unsigned 4 $ arg `shiftR` 4
      desoffset  = unsigned 4 arg
  reg0' <- getReg reg0 reg0mode reg0offset proc ram
  reg1' <- getReg reg1 reg1mode reg1offset proc ram
  des'  <- getReg des  desmode  desoffset
  writeIOReg des' `fmap` liftM2 op (readIORef reg0') (readIORef reg1')
  return Continue

regRegDesF :: (Float -> Float -> Float) -> Operation
regRegDesF op = regRegDes \i0 i1 -> unsafeCoerce $ unsafeCoerce i0 `op` unsafeCoerce i1

add, addf, and, div, divf, mul, mulf, or, sl, sr, sra, sub, subf, xor :: Operation

add = regRegDes (+)

addf = regRegDesF (+)

and = regRegDes (.&.)

div' = regRegDes div

divf = regRegDesF (/)

mul = regRegDes (*)

mulf = regRegDesF (*)

or = regRegDes (.|.)

sl = regRegDes shiftL

sr = regRegDes \i0 i1 -> fromIntegral $ (fromIntegral i0 :: Word32) `shiftR` (fromIntegral i1 :: Word32)

sra = regRegDes shiftR

sub = regRegDes (-)

subf = regRegDesF (-)

xor' = regRegDes xor

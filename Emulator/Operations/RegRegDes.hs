module Emulator.Operations.RegRegDes where

import Types
import Ubi
import Util (decode, getReg)

regRegDes :: (Int32 -> Int32 -> (Int32,Int32)) -> Operation
regRegDes op proc ram args = do
  let (ix0,     args')        = decode Unsigned 4 args
      (mode0,   args'')       = decode Unsigned 1 args'
      (offset0, args''')      = decode Unsigned 3 args''
      (ix1,     args'''')     = decode Unsigned 4 args'''
      (mode1,   args''''')    = decode Unsigned 1 args''''
      (offset1, args'''''')   = decode Unsigned 3 args'''''
      (ix2,     args''''''')  = decode Unsigned 4 args''''''
      (mode2,   args'''''''') = decode Unsigned 1 args'''''''
      (offset2, _)            = decode Unsigned 3 args''''''''
  reg0 <- getReg ix0 mode0 offset0 proc ram
  reg1 <- getReg ix1 mode1 offset1 proc ram
  des  <- getReg ix2 mode2 offset2 proc ram
  (result,extra)
  writeIOReg des `fmap` liftM2 op (readIORef reg0) (readIORef reg1)
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

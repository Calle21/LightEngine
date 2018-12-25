module Emulator.Operations.RegRegDes where

import Types
import Ubi
import Util (decode, getReg)

regRegDes :: (Int32 -> Int32 -> (Int32, Maybe Int32)) -> Operation
regRegDes op (Proc regs _) ram args = do
  let (ix0,     args')        = decode Unsigned 4 args
      (mode0,   args'')       = decode Unsigned 1 args'
      (offset0, args''')      = decode Unsigned 3 args''
      (ix1,     args'''')     = decode Unsigned 4 args'''
      (mode1,   args''''')    = decode Unsigned 1 args''''
      (offset1, args'''''')   = decode Unsigned 3 args'''''
      (ix2,     args''''''')  = decode Unsigned 4 args''''''
      (mode2,   args'''''''') = decode Unsigned 1 args'''''''
      (offset2, _)            = decode Unsigned 3 args''''''''
  reg0 <- getReg ix0 mode0 offset0 regs ram
  reg1 <- getReg ix1 mode1 offset1 regs ram
  des  <- getReg ix2 mode2 offset2 regs ram
  (result,extra) <- liftM2 op (readIORef reg0) (readIORef reg1)
  case extra of
    Nothing -> return ()
    Just v  -> writeIORef (regs ! 13) v
  writeIOReg des result
  return Continue

regRegDesF :: (Float -> Float -> Float) -> Operation
regRegDesF op = regRegDes $ extra \i0 i1 -> unsafeCoerce $ unsafeCoerce i0 `op` unsafeCoerce i1

add, addf, and, div, divf, mul, mulf, or, rl, rr, sl, sr, sra, sub, subf, xor :: Operation

add = regRegDes adde

addf = regRegDesF (+)

and = regRegDes $ extra (.&.)

div' = regRegDes dive

divf = regRegDesF $ extra (/)

mul = regRegDes mule

mulf = regRegDesF $ extra (*)

or = regRegDes (.|.)

rl = regRegDes $ extra rotateL

rr = regRegDes $ extra rotateR

sl = regRegDes $ extra shiftL

sr = regRegDes \i0 i1 -> fromIntegral $ (fromIntegral i0 :: Word32) `shiftR` (fromIntegral i1 :: Word32)

sra = regRegDes $ extra shiftR

sub = regRegDes sube

subf = regRegDesF $ extra (-)

xor' = regRegDes $ extra xor

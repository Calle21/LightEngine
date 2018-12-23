module Emulator.Operations.RegImmDes where

import Types
import Ubi
import Util (getReg)

regImmDes :: (Int32 -> Int32 -> Int32) -> Operation
regImmDes op proc ram arg = do
  let reg0       = unsigned 4 $ arg `shiftR` 23
      imm        =   signed 9 $ arg `shiftR` 14
      des        = unsigned 4 $ arg `shiftR` 10
      regmode    = unsigned 1 $ arg `shiftR` 9
      desmode    = unsigned 1 $ arg `shiftR` 8
      regoffset  = unsigned 4 $ arg `shiftR` 4
      desoffset  = unsigned 4 arg
  reg <- getReg reg0 reg0mode reg0offset proc ram
  des <- getReg des  desmode  desoffset
  regvalue <- readIORef reg
  writeIORef des (regvalue `op` immu)
  return Continue

regRegDesF :: (Float -> Float -> Float) -> Operation
regRegDesF op = regRegDes \i0 i1 -> unsafeCoerce $ unsafeCoerce i0 `op` unsafeCoerce i1

addi, andi, divi, muli, ori, xori :: Operation

addi = regImmDes (+)

andi = regImmDes (.&.)

divi = regImmDes div

muli = regImmDes (*)

ori = regImmDes (.|.)

xori = regImmDes xor

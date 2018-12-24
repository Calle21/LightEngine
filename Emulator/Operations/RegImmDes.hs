module Emulator.Operations.RegImmDes where

import Types
import Ubi
import Util (decode, getReg)

regImmDes :: (Int32 -> Int32 -> Int32) -> Operation
regImmDes op (Proc regs _) ram args = do
  let (ix0,     args')      = decode Unsigned 4 args
      (mode0,   args'')     = decode Unsigned 1 args'
      (offset0, args''')    = decode Unsigned 4 args''
      (imm,     args'''')   = decode Signed   9 args'''
      (ix1,     args''''')  = decode Unsigned 4 args''''
      (mode1,   args'''''') = decode Unsigned 1 args'''''
      (offset1, _)          = decode Unsigned 4 args''''''
  reg <- getReg ix0 mode0 offset0 proc ram
  des <- getReg ix1 mode1 offset1 proc ram
  value <- readIORef reg
  writeIORef des (value `op` imm)
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

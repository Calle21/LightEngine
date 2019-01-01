module Emulator.Operations.Li where

import Types
import Ubi
import Util (getReg, decode)

li :: Operation
li (Proc regs _) ram args = do
  let (ix,     args')    = decode Unsigned 4 args
      (mode,   args'')   = decode Unsigned 1 args'
      (offset, args''')  = decode Unsigned 4 args''
      (shift,  args'''') = decode Unsigned 5 args'''
      (imm,_)            = decode Signed  13 args''''
  des <- getReg ix mode offset regs ram
  writeIORef des (imm `shiftL` fromIntegral shift)
  return Continue

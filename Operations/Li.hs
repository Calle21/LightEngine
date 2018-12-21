module Operations.Li where

import Type
import Ubi
import Util (getReg)

li :: Operation
li mode set _ (Processor _ regs) _ arg = do
  let dest = arg `shiftR` 20
      imm  = signed 20 arg
  dest' <- getReg dest mode set regs
  writeIORef dest' imm

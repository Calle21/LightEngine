module Operations.Li where

import Types
import Ubi
import Util (getReg, signed)

li :: Operation
li _ mode set _ (Processor _ regs) _ arg = do
  let dest = arg `shiftR` 20
      imm  = signed 20 arg
  dest' <- getReg dest mode set regs
  writeIORef dest' imm
  return Continue

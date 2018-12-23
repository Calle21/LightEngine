module Emulator.Operations.Li where

import Emulator.Operations.Templates (one)
import Types

li :: Operation
li (Processor _ regs) _ arg = do
  let dest = arg `shiftR` 23
      imm  = signed 23 arg
  writeIORef dest imm
  return Continue

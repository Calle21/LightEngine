module Operations.Addi where

import Types
import Ubi
import Util (getReg, unsigned)

addi :: Operation
addi _ mode set _ (Processor _ regs) ram arg = do let reg  = arg `shiftR` 20
                                                      imm  = unsigned 15 $ arg `shiftR` 5
                                                      dest = unsigned 5 arg
                                                  reg' <- getReg reg mode set regs
                                                  reg'' <- readIORef reg'
                                                  dest' <- getReg dest mode set regs
                                                  writeIORef dest' (reg'' + imm)
                                                  return Continue

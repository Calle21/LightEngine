module Operations.Subi where

import Types
import Ubi
import Util (getReg)

subi :: Operation
subi mode set _ (Processor _ regs) ram arg = do let reg  = arg `shiftR` 20
                                                    imm  = mask 15 .&. (arg `shiftR` 5)
                                                    dest = mask 5 .&. arg
                                                reg' <- getReg reg mode set regs
                                                reg'' <- readIORef reg'
                                                dest' <- getReg dest mode set regs
                                                writeIORef dest' (reg'' - imm)

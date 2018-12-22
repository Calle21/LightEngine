module Operations.Incr where

import Types
import Ubi
import Util (getReg)

incr :: Operation
incr _ mode set _ (Processor _ regs) _ arg = do let reg = arg `shiftR` 20
                                                reg' <- getReg reg mode set regs
                                                modifyIORef reg' succ
                                                return Continue

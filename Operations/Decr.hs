module Operations.Decr where

import Types
import Ubi
import Util (getReg)

decr :: Operation
decr mode set _ (Processor _ regs) _ arg = do let reg = arg `shiftR` 20
                                              reg' <- getReg reg mode set regs
                                              modifyIORef reg' pred

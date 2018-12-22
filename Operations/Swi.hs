module Operations.Swi where

import Types
import Ubi
import Util (getReg, unsigned)

swi :: Operation
swi _ mode set _ (Processor _ regs) ram arg = do
  let reg  = arg `shiftR` 20
      addr = unsigned 20 arg
  reg' <- getReg reg mode set regs
  reg'' <- readIORef reg'
  writeIORef (ram ! addr) reg''

module Operations.Swi where

import Types
import Ubi
import Util (getReg)

swi :: Operation
swi mode set _ (Processor _ regs) ram arg = do
  let reg  = arg `shiftR` 20
      addr = mask 20 .&. arg
  reg' <- getReg reg mode set regs
  reg'' <- readIORef reg'
  writeIORef (ram ! addr) reg''

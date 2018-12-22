module Operations.Lw where

import Types
import Ubi
import Util (getReg, signed, unsigned)

lw :: Operation
lw _ mode set _ (Processor _ regs) ram arg = do
  let dest   = arg `shiftR` 20
      addr   = unsigned 5 $ arg `shiftR` 15
      offset = signed 15 arg
  dest' <- getReg dest mode set regs
  addr' <- getReg addr mode set regs
  addr'' <- readIORef addr'
  writeIORef dest' =<< readIORef (ram ! (addr'' + offset))
  return Continue

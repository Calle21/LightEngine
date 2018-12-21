module Operations.Sw where

import Types
import Ubi
import Util (getReg)

sw :: Operation
sw mode set _ (Processor _ regs) ram arg = do
  let addr   = arg `shiftR` 20
      dest   = mask 5 .&. (arg `shiftR` 15)
      offset = signed 15 arg
  addr' <- getReg addr mode set regs
  addr'' <- readIORef addr'
  dest' <- getReg dest mode set regs
  dest'' <- readIORef (proc ! dest)
  writeIORef (ram ! (dest'' + offset)) addr''

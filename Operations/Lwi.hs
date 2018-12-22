module Operations.Lwi where

import Types
import Ubi
import Util (getReg, unsigned)

lwi :: Operation
lwi _ mode set _ (Processor _ regs) ram arg = do
  let dest = arg `shiftR` 20
      addr = unsigned 20 arg
  dest' <- getReg dest mode set regs
  writeIORef dest' =<< readIORef (ram ! addr)
  return Continue

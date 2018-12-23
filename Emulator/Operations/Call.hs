module Emulator.Operations.Call where

import Types
import Ubi
import Util (unsigned)

call :: Operation
call par mode set _ (Processor _ regs) ram arg = do
  let stackSpace = arg `shiftR` 20
      addr       = unsigned 20 arg
  ic <- readIORef (proc ! 31)
  sp <- readIORef (proc ! 30)
  writeIORef (ram ! sp) ic
  modifyIORef (proc ! 30) (+stackSpace)
  writeIORef (proc ! 31) addr

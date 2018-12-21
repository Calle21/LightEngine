module Operations.Call where

import Types
import Ubi

call :: Operation
call mode set _ (Processor _ regs) ram arg = do let stackSpace = arg `shiftR` 20
                                      addr       = arg .&. mask 20
                                  ic <- readIORef (proc ! 31)
                                  sp <- readIORef (proc ! 30)
                                  writeIORef (ram ! sp) ic
                                  modifyIORef (proc ! 30) (+stackSpace)
                                  writeIORef (proc ! 31) addr

callPar :: Operation
callPar mode set procs proc ram arg =

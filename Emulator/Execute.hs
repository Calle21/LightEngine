module Emulator.Execute where

import Emulator.Processor
import Operations
import Types
import Ubi
import Util (decode)

execute :: RAM -> IO ()
execute ram = do proc <- getProcessor
                 run proc ram

run :: Processor -> RAM -> IO Int32
run proc@(Proc regs _) ram = do
  ic <- readIORef (regs ! 15)
  writeIORef (regs ! 15) (succ ic)
  fetch <- readIORef (ram ! ic)
  writeIORef (regs ! 14) fetch
  let (opi,arg) = decode 5 fetch
  sig <- (operations ! opi) proc ram arg
  case sig of
    Continue -> run proc ram
    Return   -> do returnProcessor proc
                   readIORef (regs ! 0)

module Emulator.Execute.32Bit where

import Emulator.Processor.32Bit
import Operations.32Bit
import Types.32Bit
import Ubi
import Util.32Bit (decode)

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

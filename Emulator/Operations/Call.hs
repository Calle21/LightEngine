module Emulator.Operations.Call where

import qualified Data.Map.Strict as M
import Emulator.Execute (run)
import Emulator.Processor
import Types
import Ubi
import Util (unsigned)

call :: Operation
call (Proc regs calls) ram args = do
  let (par,args') = decode Unsigned 1 args
      addr        = args' `shiftR` 7
  ic <- readIORef (regs ! 15)
  writeIORef (regs ! 15) (ic + 1)
  args'' <- ram ! ic
  let (numargs,args'') = decode Unsigned 4 args'

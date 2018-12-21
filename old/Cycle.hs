module Cycle where

import Concurrent (rPar)
import Ops
import qualified Register.Op as Op
import Types
import Ubi
import Util

cycle :: Processor -> RAM -> IO Signal
cycle proc ram = do ic <- fetch icRegFetch proc
                    (instr,_) <- fetch ic ram
                    rPar $ Op.inc ic
                    (op,args) <- fetch instr ops
                    op proc ram args

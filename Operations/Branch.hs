module Operations.Branch (branch) where

import Operations.Templates (compareImm, compareReg)
import Types
import Ubi
import Util (getReg)

branch :: Operation
branch mode set procs proc ram arg = branches ! (arg `shiftR` 22) $ mode set procs proc ram (unsigned 22 arg)

 -- Variations

branches = listArray (0,7) [al,eq,eqi,ez,ge,gei,gt,gti]

al, eq, eqi, ez, ge, gei, gt, gti :: Operation

al _ _ _ (Processor _ regs) _ arg = do modifyIORef (regs ! 31) (+ signed 22 arg)

eq = compareReg (==)

eqi = compareImm (==)

ez mode set _ (Processor _ regs) _ arg = do
  let reg  = arg `shiftR` 17
      jump = signed 17 arg
  reg' <- getReg reg mode set regs
  reg'' <- readIORef reg'
  if reg'' == 0 then modifyIORef (proc ! 31) (+ jump)
                else return ()

ge = compareReg (>=)

gei = compareImm (>=)

gt = compareReg (>)

gti = compareImm (>)

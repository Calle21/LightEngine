module Emulator.Operations (operations) where

import Operations.Branch.32bit
import Operations.Call.32bit
import Operations.Multi.32Bit
import Operations.RegImmDes.32bit
import Operations.RegRegDes.32bit

operations = listArray (0,31) [add , addf , addi , and
                             , andi , branch , call , div'
                             , divf , divi , exp' , expi
                             , la , max' , maxf , min'
                             , minf , mul , mulf , muli
                             , multi , or , ori , rl
                             , rr , sl , sr , sra
                             , sub , subf , xor' , xori]

la :: Operation
la (Proc regs _) ram args = do
  let addr = args `shiftR` 5
  writeIORef (regs ! 13) addr
  return Continue

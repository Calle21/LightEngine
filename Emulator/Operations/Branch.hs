module Emulator.Operations.Branch (branch) where

import Types
import Ubi
import Util (getReg, signed, unsigned)

branch :: Operation
branch proc ram arg = let neg  = [id,not] !! (arg `shiftR` 26)
                          fn   = types ! unsigned 4 (arg `shiftR` 22)
                          arg' = unsigned 22 arg
                      in fn neg proc ram arg'

 -- Templates

reg (Int32 -> Bool) -> (Bool -> Bool) -> Operation
reg op neg (Proc regs _) ram arg = do
  let ix   = arg `shiftR` 18
      mode = unsigned 1 `shiftR` 17
      jump = unsigned 10 arg
  reg <- getReg ix regmode 0 regs ram
  regvalue <- readIORef reg
  if neg $ op regvalue then modifyIORef (proc ! 15) (+jump)
                       else return ()
  return Continue

regImm, regReg :: (Int32 -> Int32 -> Bool) -> (Bool -> Bool) -> Operation

regImm op neg (Proc _ regs) ram arg = do
  let ix   = arg `shiftR` 18
      mode = unsigned 1 (arg `shiftR` 17)
      imm  = signed 7 (arg `shiftR` 10)
      jump = signed 10 arg
  reg <- getReg ix mode 0 regs ram
  regvalue <- readIORef reg
  if neg $ regvalue `op` imm then modifyIORef (proc ! 15) (+jump)
                             else return ()
  return Continue

regReg op neg (Proc regs _) ram arg = do
  let ix0   = arg `shiftR` 18
      ix1   = unsigned 4 (arg `shiftR` 14)
      mode0 = unsigned 1 (arg `shiftR` 13)
      mode1 = unsigned 1 (arg `shiftR` 12)
      jump  =   signed 10 arg
  reg0 <- getReg ix0 mode0 0 regs ram
  reg1 <- getReg ix1 mode1 0 regs ram
  value0 <- readIOReg reg0
  value1 <- readIORef reg1
  if neg $ value0 `op` value1 then modifyIORef (regs ! 15) (+jump)
                              else return ()
  return Continue

regRegF :: (Float -> Float -> Bool) -> Operation
regRegF op = regReg \i0 i1 -> unsafeCoerce i0 `op` unsafeCoerce i1

regRegU :: (Word32 -> Word32 -> Bool) -> Operation
regRegU op = regReg \i0 i1 -> fromIntegral i0 `op` fromIntegral i1


 -- Variations

types = listToArray [al,bit,eq,eqf,eqi,ez,gt,gtf,gti,gtu,gtz,lt,ltf,lti,ltu,ltz]

al, bit, eq, eqf, eqi, ez, gt, gtf, gti, gtu, gtz, lt, ltf, lti, ltu, ltz :: (Bool -> Bool) -> Operation

al neg (Proc regs _) ram arg = do if neg True then modifyIORef (regs ! 15) (+ signed 10 arg)
                                              else return ()
                                  return Continue

bit neg (Proc regs _) ram arg = do let ix   = arg `shiftR` 18
                                       mode = unsigned 1 (arg `shiftR` 17)
                                       bit  = unsigned 5 (arg `shiftR` 12)
                                       jump = signed 10 arg
                                   reg <- getReg ix reg 0 regs ram
                                   regvalue <- readIORef 
                                   if neg $ testBit regvalue (fromIntegral bit)
                                   then modifyIORef (regs ! 15) (+jump)
                                   else return ()
                                   return Continue

eq = regReg (==)

eqf = regRegF (==)

eqi = regImm (==)

ez = reg (==0)

gt = regReg (>)

gtf = regRegF (>)

gti = regImm (>)

gtu = regRegU (>)

gtz = reg (>0)

lt = regReg (<)

ltf = regRegF (<)

lti = regImm (<)

ltu = regRegU (<)

ltz = reg (<0)

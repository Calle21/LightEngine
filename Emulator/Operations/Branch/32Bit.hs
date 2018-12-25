module Emulator.Operations.Branch (branch) where

import Types
import Ubi
import Util (decode, getReg)

branch :: Operation
branch proc ram args = let (neg,args')  = decode Unsigned 1 args
                           (br, args'') = decode Unsigned 4 args''
                      in brTypes ! br $ ([id,not] !! neg) proc ram args''

 -- Templates

reg (Int32 -> Bool) -> (Bool -> Bool) -> Operation
reg op negfn (Proc regs _) ram args = do
  let (ix,     args')         = decode Unsigned 4 args
      (mode,   args'')        = decode Unsigned 1 args'
      (offset, args''')       = decode Unsigned 7 args''
      (jump, _)               = decode Signed  10 args'''
  reg <- getReg ix mode offset regs ram
  value <- readIORef reg
  if negfn $ op value then modifyIORef (proc ! 15) (+jump)
                      else return ()
  return Continue

regImm, regReg :: (Int32 -> Int32 -> Bool) -> (Bool -> Bool) -> Operation

regImm op negfn (Proc _ regs) ram args = do
  let (ix,     args')    = decode Unsigned 4 args
      (mode,   args'')   = decode Unsigned 1 args'
      (offset, args''')  = decode Unsigned 2 args''
      (imm,    args'''') = decode Signed   5 args'''
      (jump,_)           = decode Signed  10 args''''
  reg <- getReg ix mode offset regs ram
  value <- readIORef reg
  if negfn $ value `op` imm then modifyIORef (proc ! 15) (+jump)
                            else return ()
  return Continue

regReg op negfn (Proc regs _) ram args = do
  let (ix0,     args')      = decode Unsigned 4 args
      (mode0,   args'')     = decode Unsigned 1 args'
      (offset0, args''')    = decode Unsigned 1 args''
      (ix1,     args'''')   = decode Unsigned 4 args'''
      (mode1,   args''''')  = decode Unsigned 1 args''''
      (offset1, args'''''') = decode Unsigned 1 args'''''
      (jump,_)              = decode Signed  10 args''''''
  reg0 <- getReg ix0 mode0 offset0 regs ram
  reg1 <- getReg ix1 mode1 offset1 regs ram
  value0 <- readIOReg reg0
  value1 <- readIORef reg1
  if negfn $ value0 `op` value1 then modifyIORef (regs ! 15) (+jump)
                                else return ()
  return Continue

regRegF :: (Float -> Float -> Bool) -> Operation
regRegF op = regReg \i0 i1 -> unsafeCoerce i0 `op` unsafeCoerce i1

regRegU :: (Word32 -> Word32 -> Bool) -> Operation
regRegU op = regReg \i0 i1 -> fromIntegral i0 `op` fromIntegral i1


 -- Variations

brTypes = listToArray [al,bit,eq,eqf,eqi,ez,gt,gtf,gti,gtu,gtz,lt,ltf,lti,ltu,ltz]

al, bit, eq, eqf, eqi, ez, gt, gtf, gti, gtu, gtz, lt, ltf, lti, ltu, ltz :: (Bool -> Bool) -> Operation

al negfn (Proc regs _) ram args = do let (jump,_) = decode Signed 10 args
                                     if negfn True then modifyIORef (regs ! 15) (+jump)
                                     else return ()
                                     return Continue

bit negfn (Proc regs _) ram args = do let (ix,     args')    = decode Unsigned 4 args
                                          (mode,   args'')   = decode Unsigned 1 args'
                                          (offset, args''')  = decode Unsigned 2 args''
                                          (bit,    args'''') = decode Unsigned 5 args'''
                                          (jump, _)          = decode Signed  10 args''''
                                      reg <- getReg ix regs offset regs ram
                                      value <- readIORef reg
                                      if negfn $ testBit value (fromIntegral bit)
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

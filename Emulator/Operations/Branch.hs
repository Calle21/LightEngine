module Emulator.Operations.Branch (branch) where

import Types
import Ubi
import Util (decode, getJump, getPlace)

branch :: Operation
branch proc ram args = let (neg,args')  = decode Unsigned 1 args
                           (br, args'') = decode Unsigned 4 args'
                      in brTypes ! br $ ([id,not] !! neg) proc ram args''

 -- Templates

reg (Int64 -> Bool) -> (Bool -> Bool) -> Operation
reg op negfn (Proc regs _) ram args = do
  (reg, args') <- getPlace args regs ram
  let jump = getJump args'
  value <- readIORef reg
  if negfn $ op value then modifyIORef (proc ! 31) (+jump)
                      else return ()
  return Continue

regImm, regReg :: (Int64 -> Int64 -> Bool) -> (Bool -> Bool) -> Operation

regImm op negfn (Proc _ regs) ram args = do
  let (reg,args')  = getPlace args regs ram
      (imm,args'') = getImm   args' 10
      jump         = getJump  args''
  value <- readIORef reg
  if negfn $ value `op` imm then modifyIORef (proc ! 31) (+jump)
                            else return ()
  return Continue

regReg op negfn (Proc regs _) ram args = do
  let (reg0,args')  = getPlace args  regs ram
      (reg1,args'') = getPlace args' regs ram
      jump          = getJump  args''
  value0 <- readIOReg reg0
  value1 <- readIORef reg1
  if negfn $ value0 `op` value1 then modifyIORef (regs ! 31) (+jump)
                                else return ()
  return Continue

regRegF :: (Float -> Float -> Bool) -> Operation
regRegF op = regReg \i0 i1 -> unsafeCoerce i0 `op` unsafeCoerce i1

regRegU :: (Word32 -> Word32 -> Bool) -> Operation
regRegU op = regReg \i0 i1 -> fromIntegral i0 `op` fromIntegral i1


 -- Variations

brTypes = listToArray [al,bit,eq,eqf,eqi,ez,gt,gtf,gti,gtu,gtz,lt,ltf,lti,ltu,ltz]

al, bit, eq, eqf, eqi, ez, gt, gtf, gti, gtu, gtz, lt, ltf, lti, ltu, ltz :: (Bool -> Bool) -> Operation

al negfn (Proc regs _) _ args = do let jump = getJump args
                                   if negfn True then modifyIORef (regs ! 31) (+jump)
                                   else return ()
                                   return Continue

bit negfn (Proc regs _) ram args = do let (reg,args')  = getPlace args
                                          (bit,args'') = decode Unsigned 5 args'
                                          jump         = getJump args
                                      value <- readIORef reg
                                      if negfn $ testBit value (fromIntegral bit)
                                      then modifyIORef (regs ! 31) (+jump)
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

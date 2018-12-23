module Emulator.Operations.Branch (branch) where

import Emulator.Operations.Templates (compareImm, compareOne, compareTwo)
import Types
import Ubi
import Util (getReg, signed, unsigned)

branch :: Operation
branch par mode set procs proc ram arg =
  let fn = if arg `shiftR` 24 > 0 then not else id
  in branches ! fromIntegral (unsigned 4 $ arg `shiftR` 20) $ fn par mode set procs proc ram (unsigned 20 arg)

 -- Variations

branches = listArray (0,15) [al,eq,eqi,ez,ge,gef,gei,geu,gt,gtf,gti,gtu,gtz,lei,lti,ltz]

al, eq, eqi, ez, ge, gef, gei, geu, gt, gtf, gti, gtu, gtz, lei, lti, ltz :: (Bool -> Bool) -> Operation

al fn _ _ _ _ (Processor _ regs) _ arg = if fn True then modifyIORef (regs ! 31) (+ signed 20 arg)
                                                    else return Continue

eq = compareTwo (==)

eqi = compareImm (==)

ez = compareOne (==0)

ge = compareTwo (>=)

gef = compareTwo \i0 i1 -> (unsafeCoerce i0 :: Float) >= (unsafeCoerce i1 :: Float)

gei = compareImm (>=)

geu = compareTwo \i0 i1 -> (fromIntegral i0 :: Word32) >= (fromIntegral i1 :: Word32)

gt = compareTwo (>)

gtf = compareTwo \i0 i1 -> (unsafeCoerce i0 :: Float) > (unsafeCoerce i1 :: Float)

gti = compareImm (>)

gtu = compareTwo \i0 i1 -> (fromIntegral i0 :: Word32) > (fromIntegral i1 :: Word32)

gtz = compareOne (>0)

lei = compareImm (<=)

lti = compareImm (<)

ltz = compareOne (<0)

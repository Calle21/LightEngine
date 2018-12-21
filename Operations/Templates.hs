module Operations.Templates where

import Types
import Ubi
import Util (getReg)

compareImm, compareReg :: (Int32 -> Int32 -> Bool) -> Operation

compareImm op mode set _ (Processor _ regs) _ arg = do
  let reg  = arg `shiftR` 17
      imm  = signed 8 (arg `shiftR` 9)
      jump = signed 9 arg
  reg' <- getReg reg mode set regs
  reg'' <- readIORef reg'
  if reg'' `op` imm then modifyIORef (proc ! 31) (+ jump)
                    else return ()

compareReg op mode set _ (Processor _ regs) _ arg = do
  let r0   = arg `shiftR` 17
      r1   = unsigned 5 (arg `shiftR` 12)
      jump = signed 12 arg
  r0' <- getReg r0 mode set regs
  r0'' <- readIORef r0'
  r1' <- getReg r1 mode set regs
  r1'' <- readIORef r1'
  if r0'' `op` r1'' then modifyIORef (proc ! 31) (+ jump)
                    else return ()

twoOne :: (Int32 -> Int32 -> Int32) -> Operation
twoOne op mode set _ (Processor _ regs) _ arg = do
  let r0   = arg `shiftR` 20
      r1   = mask 5 .&. (arg `shiftR` 15)
      dest = mask 5 .&. (arg `shiftR` 10)
  r0' <- getReg r0 mode set regs
  r0'' <- readIORef r0'
  r1' <- getReg r1 mode set regs
  r1'' <- readIORef r1'
  dest' <- getReg dest mode set regs
  writeIORef dest' (r0'' `op` r1'')

oneOne :: (Int32 -> Int32) -> Operation
oneOne op mode set _ (Processor _ regs) _ arg = do
  let reg  = arg `shiftR` 20
      dest = mask 5 .&. (arg `shiftR` 15)
  reg' <- getReg reg mode set regs
  reg'' <- readIORef reg'
  dest' <- getReg dest mode set regs
  writeIORef dest' (op reg'')

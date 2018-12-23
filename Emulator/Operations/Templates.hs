module Emulator.Operations.Templates where

import Types
import Ubi
import Util (getReg, signed, unsigned)

compareImm, compareTwo :: (Int32 -> Int32 -> Bool) -> (Bool -> Bool) -> Operation

compareImm op fn _ mode set _ (Processor _ regs) _ arg = do
  let reg  = arg `shiftR` 15
      imm  = signed 7 (arg `shiftR` 8)
      jump = signed 8 arg
  reg' <- getReg reg mode set regs
  reg'' <- readIORef reg'
  if fn $ reg'' `op` imm then modifyIORef (proc ! 31) (+ jump)
                         else return Continue

compareOne op fn _ mode set _ (Processor _ regs) _ arg = do
  let reg  = arg `shiftR` 15
      jump = signed 15 arg
  reg' <- getReg reg mode set regs
  reg'' <- readIORef reg'
  if fn $ op reg'' then modifyIORef (proc ! 31) (+ jump)
                   else return Continue

compareTwo op fn _ mode set _ (Processor _ regs) _ arg = do
  let r0   = arg `shiftR` 15
      r1   = unsigned 5 (arg `shiftR` 10)
      jump = signed 10 arg
  r0' <- getReg r0 mode set regs
  r0'' <- readIORef r0'
  r1' <- getReg r1 mode set regs
  r1'' <- readIORef r1'
  if fn $ r0'' `op` r1'' then modifyIORef (proc ! 31) (+ jump)
                         else return Continue

imm :: (Int32 -> Int32 -> Int32) -> Operation
imm op _ mode set _ (Processor _ regs) ram arg = do let reg  = arg `shiftR` 20
                                                        imm  = unsigned 15 $ arg `shiftR` 5
                                                        dest = unsigned 5 arg
                                                    reg' <- getReg reg mode set regs
                                                    reg'' <- readIORef reg'
                                                    dest' <- getReg dest mode set regs
                                                    writeIORef dest' (reg'' `op` imm)
                                                    return Continue

one :: (Int32 -> Int32) -> Operation
one op _ mode set _ (Processor _ regs) _ arg = do let reg = arg `shiftR` 20
                                                  reg' <- getReg reg mode set regs
                                                  modifyIORef reg' op
                                                  return Continue

oneOne :: (Int32 -> Int32) -> Operation
oneOne op _ mode set _ (Processor _ regs) _ arg = do
  let reg  = arg `shiftR` 20
      dest = unsigned 5 (arg `shiftR` 15)
  reg' <- getReg reg mode set regs
  reg'' <- readIORef reg'
  dest' <- getReg dest mode set regs
  writeIORef dest' (op reg'')
  return Continue

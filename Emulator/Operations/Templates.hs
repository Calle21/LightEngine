module Emulator.Operations.Templates where

import Types
import Ubi
import Util (getReg, signed, unsigned)

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

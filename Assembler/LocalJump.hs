module Assembler.LocalJump where

type LocalJumped = [LJInstr]

localJump :: (String,Replaced) -> (String,LocalJumped)
localJump (filename, (regs,labs,instr)) = (filename, (regs, rec 0 instr))
  where
  rec :: Int -> [[Token]] -> [[A4Tok]]
  rec i (x:xs) = 

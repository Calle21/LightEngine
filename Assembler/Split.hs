module Assembler.Split where

import Types
import Ubi
import Util

split :: (FilePath, RegList, [[Token]]) -> (FilePath, RegList, [[Token]], [[Token]])
split (filename, regs, file) = let (c,d) = partition isDataLine file
                               in (filename, regs, c, d)

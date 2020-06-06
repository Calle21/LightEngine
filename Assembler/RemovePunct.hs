module Assembler.RemovePunct where

import Types
import Ubi
import Util

removePunct :: (FilePath, RegList, [[Token]], [[Token]]) -> (FilePath, RegList, [[Token]], [[Token]])
removePunct (filename, regs, dat, text) = (filename, regs, dat, map (withoutIf punct) text)

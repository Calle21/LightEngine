module Assembler.RemovePunct where

import Share
import Types
import Ubi

removePunct :: (FilePath, [[Token]], [[Token]]) -> (FilePath, [[Token]], [[Token]])
removePunct (filename, dat, text) = (filename, dat, map (withoutIf punct) text)

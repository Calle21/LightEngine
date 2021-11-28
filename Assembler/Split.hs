module Assembler.Split where

import Share
import Types
import Ubi

split :: (FilePath, [[Token]]) -> (FilePath, [[Token]], [[Token]])
split (filename, file) = let (b,c) = partition isDataLine file
                         in (filename, b, c)

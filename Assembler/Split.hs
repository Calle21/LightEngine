module Assembler.Split where

import Types
import Ubi (partition)
import Util (isDataLine)

split :: (FilePath, Checked) -> (FilePath, Split)
split (filename, file) = (filename, partition isDataLine file)

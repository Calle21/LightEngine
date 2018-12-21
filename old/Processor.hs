module Processor where

import Types
import Ubi

processor :: IO Processor
processor = do reg <- replicateM (register 20)
               r <- register 5
               binTree (reg ++ [r])

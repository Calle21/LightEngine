module Processor where

import Types
import Ubi

processor :: IO Processor
processor = liftM (listArray (0,7)) (replicateM 8 register)

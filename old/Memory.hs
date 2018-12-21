module Memory where

import Register
import Types
import Ubi

ram :: Int -> IO Ram
ram addrWidth = binTree `fmap` replicateM (2 ^ addrWidth) register

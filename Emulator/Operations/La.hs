module Emulator.Operations.La where

import Types
import Ubi

la :: Operation
la (Proc regs _) ram args = do writeIORef (proc ! 13) $ fromIntegral $ (fromIntegral args :: Word32) `shiftR` 5
                               return Continue

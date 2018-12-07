module Register where

import Types
import Ubi

register :: IO Register
register = liftM (listArray (0,19)) (replicateM 20 (newIORef False))

readReg :: Register -> IO Word32
readReg reg = loop 0
  where
  loop :: Int -> IO Word32
  loop 20 = return 0
  loop n  = do next <- (`shiftL` 1) <$> loop (n + 1)
               val  <- readIORef (reg ! n)
               return (next + boolToInt val)

writeReg :: Register -> Word32 -> IO ()
writeReg reg val = loop val 0
  where
  loop :: Word32 -> Int -> IO ()
  loop _   20 = return ()
  loop val n  = do writeIORef (reg ! n) (odd val)
                   loop (val `shiftR` 1) (n + 1)

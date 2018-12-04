module Register where

import Ubi

type Processor = Array Int Register
type Register = Array Int (IORef Bool)

register :: IO Register
register = liftM (listArray (0,35)) (replicateM 36 (newIORef False))

processor :: IO Processor
processor = liftM (listArray (0,7)) (replicateM 8 register)

readReg :: Register -> IO Int64
readReg reg = loop 0
  where
  loop :: Int -> IO Int64
  loop 36 = return 0
  loop n  = do next <- (`shiftL` 1) <$> loop (n + 1)
               val  <- readIORef (reg ! n)
               return (next + boolToInt val)

writeReg :: Register -> Int64 -> IO ()
writeReg reg val = loop val 0
  where
  loop :: Int64 -> Int -> IO ()
  loop _   36 = return ()
  loop val n  = do writeIORef (reg ! n) (odd val)
                   loop (val `shiftR` 1) (n + 1)

module Register where

import Types
import Ubi

stackRegFetch = unsafePerformIO $ register 2
icRegFetch = unsafePerformIO do reg <- register 2
                                writeIORef (reg !! 1) True
                                return reg
addrRegFetch = unsafePerformIO do reg <- register 2
                                  writeIORef (reg !! 0) True
                                  return reg
offRegFetch = unsafePerformIO do reg <- register 2
                                 writeIORef (reg !! 0) True
                                 writeIORef (reg !! 1) True
                                 return reg

register :: Int -> IO Register
register len = replicateM len (newIORef False)

readReg :: Register -> IO Word32
readReg reg = loop 0
  where
  loop :: Int -> IO Word32
  loop 20 = return 0
  loop n  = do next <- (`shiftL` 1) <$> loop (n + 1)
               val  <- readIORef (reg ! n)
               return $ next .|. fromEnum val

writeReg :: Register -> Word32 -> IO ()
writeReg reg val = loop val 0
  where
  loop :: Word32 -> Int -> IO ()
  loop _   20 = return ()
  loop val n  = do writeIORef (reg ! n) (odd val)
                   loop (val `shiftR` 1) (n + 1)

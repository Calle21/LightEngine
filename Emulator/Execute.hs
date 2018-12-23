module Emulator.Execute where

import Operations
import Types
import Ubi
import Util (pop, push)

execute :: RAM -> IO ()
execute ram = do
  proc <- makeProcessor
  run proc ram

makeProcessor :: IO Processor
makeProcessor = listToArray `fmap` replicateM 16 (newIORef 0)

run :: Processor -> RAM -> IO Int32
run proc ram = do
  ic <- readIORef (proc ! 15)
  writeIORef (proc ! 15) (ic + 1)
  fetch <- readIORef (ram ! ic)
  writeIORef (proc ! 14) fetch
  let (op,arg) = decode fetch
  sig <- op proc ram arg
  case sig of
    Continue -> run proc ram
    Return   -> readIORef (proc ! 0)
  where
  decode :: Int32 -> (Operation,Int32)
  decode fetch = (operations ! fromIntegral (unsigned 5 $ fetch `shiftR` 27)
                , unsigned 27 fetch)

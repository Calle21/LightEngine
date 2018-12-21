module Execute where

import Operations
import Ubi

execute :: Executable -> IO ()
execute (Executable entry ram) = do procs <- liftM2 Stack (newIORef 4) (listArray (0,3) `fmap` mapM makeProcessor [0..3])
                                    proc <- pop procs
                                    writeIORef (proc ! 31) entry
                                    run proc procs ram
  where
  makeProcessor :: Int -> IO Processor
  makeProcessor i = Processor i `fmap` (listArray (0,31) `fmap` replicateM 32 (newIORef 0))

run :: Processor -> Stack -> RAM -> IO ()
run proc@(Processor _ regs) procs ram = do
  ic <- readIORef (regs ! 31)
  writeIORef (regs ! 31) (ic + 1)
  fetch <- readIORef (ram ! ic)
  let (op,arg) = decode fetch
      op'      = operations ! op
  sig <- op' procs proc ram arg
  case sig of
    Continue -> run proc procs ram
    Exit     -> push proc procs
  where
  decode :: Int32 -> (Int32,Int32)
  decode i = let op  = mask 6 .&. (i `shiftR` 26)
                 arg = mask 26 .&. i
             in (op,arg)

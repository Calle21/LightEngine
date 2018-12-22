module Execute where

import Operations
import Types
import Ubi
import Util (pop, push)

execute :: Executable -> IO ()
execute (Executable ram) = do
  procs <- liftM2 Pool (newIORef 4) (listArray (0,3) `fmap` mapM makeProcessor [0..3])
  proc <- pop procs
  writeIORef (proc ! 31) 1000
  run proc procs ram
  where
  makeProcessor :: Int -> IO Processor
  makeProcessor i = Processor i `fmap` (listArray (0,31) `fmap` replicateM 32 (newIORef 0))

run :: Processor -> Pool -> RAM -> IO ()
run proc@(Processor _ regs) procs ram = do
  ic <- readIORef (regs ! 31)
  writeIORef (regs ! 31) (ic + 1)
  fetch <- readIORef (ram ! ic)
  let (par,op,mode,arg) = decode fetch
  sig <- op par mode ... procs proc ram arg
  case sig of
    Continue -> run proc procs ram
    Exit     -> push proc procs
  where
  decode :: Int32 -> (Bool,Operation,Mode,Int32)
  decode fetch = let par  = toEnum $ unsigned 1 (fetch `shiftR` 31)
                     op   = operations ! fromIntegral (unsigned 5 $ fetch `shiftR` 26)
                     mode = toEnum $ unsigned 1 (fetch `shiftR` 25)
                     arg  = unsigned 25 fetch
                 in (par,op,mode,arg)

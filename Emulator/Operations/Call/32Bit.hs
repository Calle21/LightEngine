module Emulator.Operations.Call where

import qualified Data.Map.Strict as M
import Emulator.Execute (run)
import Emulator.Processor
import Types
import Ubi
import Util (unsigned)

call :: Operation
call (Proc regs calls) ram addr = do
  newproc <- getProcessor
  writeIORef (newproc ! 15) addr
  ic <- readIORef (proc ! 15)
  writeIORef (proc ! 15) (ic + 2)
  ixs  <- readIORef (ram ! ic)
  modes <- readIORef (ram ! ic + 1)
  let offsets = modes `shiftL` 8
  (des, ixs', modes', offsets') <- loop ixs modes offset regs ram
  move ixs' (shiftL info 4) proc newproc ram
  modifyIORef calls $ M.insert ixRet (run newproc ram)
  return Continue
  where
  move :: Int32 -> Int32 -> Processor -> Processor -> RAM -> IO ()
  move ixs info proc newproc ram = loop 1 ixs info (shiftL info 7)
    where
    loop :: Int32 -> Int32 -> Int32 -> Int32 -> IO ()
    loop 8 _   _     _       = return ()
    loop i ixs modes offsets = do let (ix,ixs')          = decode 4 ixs
                                      (mode,modes')      = decode 1 modes
                                      (offset, offsets') = decode 3 offsets
                                  argreg <- getReg ix mode offset proc ram
                                  value <- readIOReg argreg
                                  writeIORef (newproc ! i) value
                                  loop (i + 1) ixs' modes' offset'

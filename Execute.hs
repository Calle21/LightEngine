module Execute where

import Register

exec :: Executable -> IO ()
exec exe = do
  proc   <- processor
  coproc <- processor
  loop exe proc coproc
  where
  loop :: Executable -> Processor -> Processor -> IO ()
  loop exe proc coproc = do
    ic <- getIc proc

getIc :: Processor -> IO Word32
getIc proc = readReg (proc ! 7)

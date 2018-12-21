module Execute where

import Processor
import Types

execute :: Executable -> IO ()
execute (Executable start ram) = do
  proc <- processor
  writeReg (proc ! 1) start
  loop proc ram
  where
  loop :: Processor -> RAM -> IO ()
  loop proc ram = do sig <- cycle proc ram
                     case sig of
                       Continue -> loop proc ram
                       Exit     -> return ()

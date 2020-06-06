module Assembler.Prepare where

import Types
import Ubi
import Util

prepare :: (Int64, [Int64]) -> IO (RAM, Proc)
prepare (main,ls) = do hSetBuffering stdout NoBuffering
                       ls'  <- mapM newIORef ls
                       proc <- getProcessor
                       writeIORef (proc ! 15) main
                       return (listArray (0,fromIntegral $ length ls - 1) ls', proc)

getProcessor :: IO Proc
getProcessor = do ls <- replicateM 16 $ newIORef 0
                  return $ listArray (0,15) ls

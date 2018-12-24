module Emulator.Processor (getProcessor, returnProcessor) where

import qualified Data.Map.Strict as M
import Types
import Ubi
import Util (listToArray)

getProcessor :: IO Processor
getProcessor = do stack <- readIORef processors
                  if null stack
                  then return $ Proc (listToArray `fmap` replicateM 16 (newIORef 0))
                                     (newIORef M.empty)
                  else do writeIORef processors (tail stack)
                          return $ head stack

returnProcessor :: Processor -> IO ()
returnProcessor p = modifyIORef processors (p :)

processors :: IORef [Processor]
processors = unsafePerformIO (newIORef [])

module Emulator.Operations.Wait.32Bit where

import qualified Data.Map.Strict as M
import Types.32Bit
import Util.32Bit (decode)

wait :: Operation
wait (Proc regs callIO) _ args = do let (ix,_) = decode Unsigned 4 args
                                    calls <- readIORef callIO
                                    case M.lookup ix calls of
                                      Nothing   -> error "No call waiting"
                                      Just call -> do writeIORef callIO $ M.remove ix calls
                                                      result <- call
                                                      writeIORef (proc ! ix) result
                                                      return Continue

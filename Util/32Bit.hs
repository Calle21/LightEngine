module Util.32Bit where

import Types.32Bit
import Ubi
import Util

decode :: DecodeType -> Int -> Int32 -> (Int32,Int32)
decode Unsigned amount value = (shiftR value (32 - amount), shiftL value amount)
decode Signed   amount value = let sign    = value .&. minBound
                                   cleared = clearBit value 31
                               in (sign .|. cleared `shiftR` (32 - amount), shiftL value amount)

getReg :: Int32 -> Int32 -> Int32 -> Regs -> RAM -> IO Register
getReg ix 0 _      regs _   = return $ regs ! ix
getReg ix 1 offset regs ram = do addr <- (+offset) `fmap` readIORef (regs ! ix)
                                 return $ ram ! addr
                              
listToArray :: [a] -> Array Int32 a
listToArray xs = listArray (0, length xs - 1) xs

module Util.32Bit where

import Types.32Bit
import Ubi
import Util

decode :: DecodeType -> Int -> Int32 -> (Int32,Int32)
decode Unsigned amount value = (shiftR value (32 - amount), shiftL value amount)
decode Signed   amount value = let sign    = value .&. minBound
                                   cleared = clearBit value 31
                               in (sign .|. cleared `shiftR` (32 - amount), shiftL value amount)

getPlace :: Int -> Int32 -> Regs -> Ram -> IO (Reg, Int32)
getPlace offsetAmount args regs ram = do
  let (ix,     args')   = decode Unsigned 4            args
      (mode,   args'')  = decode Unsigned 1            args'
      (offset, args''') = decode Unsigned offsetAmount args''
  reg <- get ix mode offset
  return (reg, args''')
  where
  get :: Int32 -> Int32 -> Int32 -> IO Register
  get ix 0 _      regs _   = return $ regs ! ix
  get ix 1 offset regs ram = do addr <- (+offset) `fmap` readIORef (regs ! ix)
                                return $ ram ! addr
                              
listToArray :: [a] -> Array Int32 a
listToArray xs = listArray (0, length xs - 1) xs

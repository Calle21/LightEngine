module Util where

import Ubi

type Operation = Word32 -> IO ()

binSearch :: C.ByteString -> Array Int (C.ByteString, Operation) -> Maybe Operation
binSearch s arr = let (lo,hi) = bounds arr
                  in rec lo hi
  where
  rec :: Int -> Int -> Maybe Operation
  rec lo hi = if hi < lo then Nothing
              else let mid     = (hi + lo) `div` 2
                       (s',op) = arr ! mid
                   in case compare s' s of
                        EQ  -> Just op
                        LT  -> rec lo (mid - 1)
                        GT  -> rec (mid + 1) hi

bitSet :: Bits a => a -> Int -> Bool -> a
bitSet b n v = (if v then setBit else clearBit) b n

boolToInt :: Bool -> Word32
boolToInt True = 1
boolToInt _    = 0

decode :: Word32 -> Word32 -> (Word32, Word32)
decode amount val = let code = mask amount .&. val
                        new  = val `shiftR` amount
                    in (code, new)

decodes :: Word32 -> Word32 -> Word32 -> ([Word32], Word32)
decodes many amount val = loop [] many val
  where
  loop :: [Word32] -> Word32 -> Word32 -> ([Word32], Word32)
  loop acc 0 val = (reverse acc, val)
  loop acc n val = let (code,new) = decode amount val
                   in loop (code : acc) (n - 1) new

intToBool :: Int -> Bool
intToBool 0 = False
intToBool 1 = True

listToArray :: [a] -> Array Int a
listToArray xs = listArray (0,length xs - 1) xs

mask :: Word32 -> Word32
mask amount = 2 ^ amount - 1

module Assembler.Compact where

import Assembler.OpInfo
import Share
import Types
import Ubi

compact :: (Int64, [Int64], [[Int64]]) -> (Int64, [Int64])
compact (main,dat,text) = (main, dat ++ processText text)
  where
  processText :: [[Int64]] -> [Int64]
  processText ((op:args):xs) = pack op args (getPackSyntax op) : processText xs
    where
    pack :: Int64 -> [Int64] -> [Int64] -> Int64
    pack op args syn = let ret = op `shiftL` 58
                       in rec ret 58 args syn
      where
      rec :: Int64 -> Int64 -> [Int64] -> [Int64] -> Int64
      rec ret off (x:xs) (y:ys) = let x'   = x .&. (2 ^ y - 1) -- In case it's a negative number
                                      x''  = x' `shiftL` fromIntegral (off - y)
                                      ret' = ret .|. x''
                                  in rec ret' (off - y) xs ys
      rec ret _   []     []     = ret
  processText [] = []

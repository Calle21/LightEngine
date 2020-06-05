module Assembler.Compact where

import Assembler.OpInfo
import Types
import Ubi

compact :: Clean -> Compact
compact (main,dat,text) = (main, dat ++ processText text)
  where
  processText :: [[Int64]] -> [Int64]
  processText ((op:args):xs) = pack op args (getPackSyntax op) : processText xs
    where
    pack :: Int64 -> [Int64] -> [Int64] -> Int64
    pack op args syn = let ret = op `shiftL` 59
                       in rec ret 59 args syn
      where
      rec :: Int64 -> Int64 -> [Int64] -> [Int64] -> Int64
      rec ret off (x:xs) (y:ys) = let x'   = x .&. (2 ^ y - 1)
                                      x''  = x' `shiftL` fromIntegral (off - y)
                                      ret' = ret .|. x''
                                  in rec ret' (off - y) xs ys
      rec ret _   []     []     = ret
  processText [] = []

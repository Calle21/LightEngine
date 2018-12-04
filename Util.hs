module Util where

import Ubi

type Operation = Int64 -> IO ()

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

boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt _    = 0

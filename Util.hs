module Util where

import Types
import Ubi

aerror :: String -> Int -> String -> t
aerror filename line message = error ("Error in file " ++ filename ++ " on line " ++ show line ++ ".\n" ++ message)

decode :: Int64 -> Int64 -> (Int64,Int64)
decode amount i = let i' | amount <= 5 = fromIntegral $ (fromIntegral i :: Word64) `shiftR` fromIntegral (64 - amount)
                         | otherwise   = i `shiftR` fromIntegral (64 - amount)
                  in (i', i `shiftL` fromIntegral amount)

isDataLine :: [Token] -> Bool
isDataLine [a,b] | isLab a && isData b = True
isDataLine _     = False

isLab :: Token -> Bool
isLab (Llab _) = True
isLab (Glab _) = True
isLab _        = False

isData :: Token -> Bool
isData (INum _)  = True
isData (Space _) = True
isData (Str _)   = True
isData _         = False

punct :: Token -> Bool
punct Comma = True
punct Arrow = True
punct _     = False

withoutIf :: (a -> Bool) -> [a] -> [a]
withoutIf fn (x:xs) | fn x      = withoutIf fn xs
                    | otherwise = x : withoutIf fn xs
withoutIf _  []     = []

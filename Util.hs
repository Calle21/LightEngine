module Util where

import Types
import Ubi

aerror :: String -> Int -> String -> t
aerror filename line message = error ("Error in file " ++ filename ++ " on line " ++ show line ++ ".\n" ++ message)

decode :: Int64 -> Int64 -> (Int64,Int64)
decode amount i = let i' | amount <= 5 = fromIntegral $ (fromIntegral i :: Word64) `shiftR` fromIntegral (64 - amount)
                         | otherwise   = i `shiftR` fromIntegral (64 - amount)
                  in (i', i `shiftL` fromIntegral amount)

isData :: Token -> Bool
isData (INum _)  = True
isData (Space _) = True
isData (Str _)   = True
isData _         = False

isDataLine :: [Token] -> Bool -- Check, Split
isDataLine [a,b] | isLab a && isData b = True
isDataLine _     = False

isLab :: Token -> Bool
isLab (Llab _) = True
isLab (Glab _) = True
isLab _        = False

isLabLine :: [Token] -> Bool -- Check, Tidy
isLabLine [Llab _] = True
isLabLine [Glab _] = True
isLabLine _        = False

punct :: Token -> Bool -- GetRegList, RemovePunct
punct Comma = True
punct Arrow = True
punct _     = False

syscalls :: [String]
syscalls = ["pChar", "pString", "pInt", "pUnsigned", "rChar", "rString", "rInt", "rUnsigned"]

getSyscall :: String -> Maybe Int64 -- Tidy
getSyscall s = liftM fromIntegral $ s `elemIndex` syscalls

withoutIf :: (a -> Bool) -> [a] -> [a] -- GetRegList, RemovePunct
withoutIf fn (x:xs) | fn x      = withoutIf fn xs
                    | otherwise = x : withoutIf fn xs
withoutIf _  []     = []

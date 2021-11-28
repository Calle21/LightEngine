module Share where

import Types
import Ubi

aerror :: String -> Int -> String -> t
aerror filename line message = error ("Error in file " ++ filename ++ " on line " ++ show line ++ ".\n" ++ message)

decode :: Int64 -> Int64 -> (Int64,Int64)
decode amount i = let i' | amount <= 6 = fromIntegral $ (fromIntegral i :: Word64) `shiftR` fromIntegral (64 - amount)
                         | otherwise   = i `shiftR` fromIntegral (64 - amount)
                  in (i', i `shiftL` fromIntegral amount)

getSyscall :: String -> Maybe Int64
getSyscall s = liftM fromIntegral $ s `elemIndex` syscalls

isData :: Token -> Bool
isData (INum _)  = True
isData (Space _) = True
isData (Str _)   = True
isData (FNum _)  = True
isData _         = False

isDataLine :: [Token] -> Bool
isDataLine [a,b] | isLab a && isData b = True
isDataLine _     = False

isLab :: Token -> Bool
isLab (Llab _) = True
isLab (Glab _) = True
isLab _        = False

isLabLine :: [Token] -> Bool
isLabLine [Llab _] = True
isLabLine [Glab _] = True
isLabLine _        = False

punct :: Token -> Bool
punct Comma = True
punct Arrow = True
punct _     = False

regs :: [(String,Int64)]
regs = [("r0",0)
       ,("r1",1)
       ,("r2",2)
       ,("r3",3)
       ,("r4",4)
       ,("r5",5)
       ,("r6",6)
       ,("r7",7)
       ,("r8",8)
       ,("r9",9)
       ,("r10",10)
       ,("r11",11)
       ,("sp",12)
       ,("link",13)
       ,("fetch",14)
       ,("ic",15)]

syscalls :: [String]
syscalls = ["pChar", "pString", "pInt", "pFloat", "rChar", "rString", "rInt", "rFloat"]

withoutIf :: (a -> Bool) -> [a] -> [a]
withoutIf fn (x:xs) | fn x      = withoutIf fn xs
                    | otherwise = x : withoutIf fn xs
withoutIf _  []     = []

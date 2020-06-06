module Assembler.OpInfo where

import Execute.Operations
import Types
import Ubi
import Util

getOpi :: String -> Int64
getOpi s = rec 0 opInfo
  where
  rec :: Int64 -> [OpInfo] -> Int64
  rec count ((xs,_,_):ys) = case recRec count xs of
                              Left i  -> rec i ys
                              Right i -> i
    where
    recRec :: Int64 -> [(String,ExeFunc)] -> Either Int64 Int64
    recRec count (x:xs) | s == fst x = Right count
                        | otherwise  = recRec (count + 1) xs
    recRec count []     = Left count

getPackSyntax :: Int64 -> [Int64]
getPackSyntax op = rec op opInfo
  where
  rec :: Int64 -> [OpInfo] -> [Int64]
  rec count ((xs,_,syn):ys) = let count' = count - fromIntegral (length xs)
                              in if count' < 0 then syn
                                 else rec count' ys

fetchOperation :: Int64 -> ExeFunc
fetchOperation i = rec i opInfo
  where
  rec :: Int64 -> [OpInfo] -> ExeFunc
  rec i (x:xs) = case get i x of
                   Right fn -> fn
                   Left i'  -> rec i' xs
    where
    get :: Int64 -> OpInfo -> Either Int64 ExeFunc
    get i (xs,_,_) | i < fromIntegral (length xs) = Right $ snd $ xs !! fromIntegral i
                   | otherwise                    = Left (i - fromIntegral (length xs))

getOpSyntax :: String -> Maybe [Syn]
getOpSyntax s = rec opInfo
  where
  rec :: [OpInfo] -> Maybe [Syn]
  rec ((xs,syn,_):ys) | s `elem1` xs = Just syn
                      | otherwise    = rec ys
    where
    elem1 :: String -> [(String,ExeFunc)] -> Bool
    elem1 s0 ((s1,_):xs) | s0 == s1  = True
                         | otherwise = elem1 s0 xs
    elem1 _  []          = False
  rec [] = Nothing

opInfo :: [OpInfo]
opInfo = [([("not", regReg complement)
           ,("move", regReg id)]
           ,[RG,RG]
           ,[4,4])
         ,([("add", regRegReg (+))
           ,("and", regRegReg (.&.))
           ,("div", regRegReg div)
           ,("mul", regRegReg (*))
           ,("or", regRegReg (.|.))
           ,("sl", regRegReg (\i0 i1 -> i0 `shiftL` fromIntegral i1))
           ,("sr", regRegReg (\i0 i1 -> fromIntegral $ (fromIntegral i0 :: Word64) `shiftR` fromIntegral i1))
           ,("sra", regRegReg (\i0 i1 -> i0 `shiftR` fromIntegral i1))
           ,("sub", regRegReg (-))
           ,("xor", regRegReg xor)]
           ,[RG,RG,RG]
           ,[4,4,4])
         ,([("li",li)],[IA,RG],[55,4])
         ,([("addi", regImmDes (+))
           ,("andi", regImmDes (.&.))
           ,("ori", regImmDes (.|.))
           ,("sli", regImmDes (\i0 i1 -> i0 `shiftL` fromIntegral i1))
           ,("sri", regImmDes (\i0 i1 -> fromIntegral $ (fromIntegral i0 :: Word64) `shiftR` fromIntegral i1))
           ,("srai", regImmDes (\i0 i1 -> i0 `shiftR` fromIntegral i1))
           ,("xori", regImmDes xor)]
           ,[RG,IM,RG]
           ,[4,51,4])
         ,([("lw", lw)],[PL,RG],[4,51,4])
         ,([("sw", sw)],[RG,PL],[4,4,51])
         ,([("b",b)],[LB],[59])
         ,([("br",br)],[RG],[4])
         ,([("beq", bRegReg (==))
           ,("bgt", bRegReg (<))
           ,("bge", bRegReg (<=))]
           ,[RG,RG,LB]
           ,[4,4,51])
         ,([("beqi", bRegImm (==))
           ,("bgti", bRegImm (<))
           ,("bgei", bRegImm (<=))]
           ,[RG,IM,LB]
           ,[4,16,39])
         ,([("exit",exit)],[],[])
         ,([("syscall",syscall)],[SC],[59])]

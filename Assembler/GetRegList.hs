module Assembler.GetRegList (getRegList) where

import Types
import Ubi
import Util

getRegList :: (FilePath, [[Token]]) -> (FilePath, RegList, [[Token]])
getRegList (filename, file) | null file = aerror filename 0 "Empty file :("
                            | otherwise = let (regs,file') = dropIt file
                                          in (filename, regs, file')

dropIt :: [[Token]] -> (RegList, [[Token]])
dropIt (x:xs) = let (regLine,file) | regListSyntax x = (withoutIf punct x,xs)
                                   | otherwise       = ([],x:xs)
                in (getem 0 regLine, file)

regListSyntax :: [Token] -> Bool
regListSyntax [Name _]          = True
regListSyntax (Name _:Comma:xs) = regListSyntax xs
regListSyntax _                 = False

getem :: Int64 -> [Token] -> RegList
getem i (Name s:xs) = (s,i) : getem (i + 1) xs
getem _ []          = [("r0",0)
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

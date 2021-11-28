module Assembler.Clean where

import Assembler.OpInfo
import Share
import Types
import Ubi

clean :: (SymTable, [Token], [[Token]]) -> (Int64, [Int64], [[Int64]])
clean (sym,dat,text) = let dat'  = replicate 1024 0 ++ cleanData dat
                           text' = cleanText (fromIntegral $ length dat') text
                       in case "main" `lookup` sym of
                            Just i  -> (i, dat', text')
                            Nothing -> error "Couldn't find main"
  where
  cleanData :: [Token] -> [Int64]
  cleanData (INum i:xs)  = i : cleanData xs
  cleanData (Space i:xs) = replicate (fromIntegral i) 0 ++ cleanData xs
  cleanData (Str s:xs)   = map (fromIntegral . ord) s ++ cleanData xs
  cleanData (FNum f:xs)  = unsafeCoerce f : cleanData xs
  cleanData []           = []
  cleanText :: Int64 -> [[Token]] -> [[Int64]]
  cleanText pos ((Name s:args):xs) = (getOpi s : cleanArgs args) : cleanText (pos + 1) xs
    where
    cleanArgs :: [Token] -> [Int64]
    cleanArgs (x:xs) = case x of
                         Reg i       -> i : cleanArgs xs
                         INum i      -> i : cleanArgs xs
                         Name s      -> case s `lookup` sym of
                                          Just i  -> (i - pos - 1) : cleanArgs xs
                                          Nothing -> error ("Couldn't find lab : " ++ s)
                         Addr s      -> case s `lookup` sym of
                                          Just i  -> i : cleanArgs xs
                                          Nothing -> error ("Couldn't find lab for address : " ++ s)
                         Place i0 i1 -> i0 : i1 : cleanArgs xs
    cleanArgs [] = []
  cleanText _ [] = []

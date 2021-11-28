module Assembler.GetLabs where

import Share
import Types
import Ubi

getLabs :: ([[Token]],[[Token]]) -> (SymTable, [Token], [[Token]])
getLabs (dat, text) = let (dataLabs,dat',dataSize) = getDataLabs [] [] 1024 dat
                          (textLabs,text')         = getTextLabs [] [] dataSize text
                      in (dataLabs ++ textLabs, dat', text')
  where
  getDataLabs :: [Token] -> SymTable -> Int64 -> [[Token]] -> (SymTable, [Token], Int64)
  getDataLabs acc0 acc1 size ([Glab s, d]:xs) = case d of
                                                  INum i  -> getDataLabs (d : acc0) ((s,size) : acc1) (size + 1) xs
                                                  Space i -> getDataLabs (d : acc0) ((s,size) : acc1) (size + i) xs
                                                  Str s'  -> getDataLabs (d : acc0) ((s,size) : acc1) (size + fromIntegral (length s')) xs
                                                  FNum f  -> getDataLabs (d : acc0) ((s,size) : acc1) (size + 1) xs
  getDataLabs acc0 acc1 size []               = (acc1,reverse acc0,size)
  getTextLabs :: [[Token]] -> SymTable -> Int64 -> [[Token]] -> (SymTable,[[Token]])
  getTextLabs acc0 acc1 size (x:xs) = case x of
                                        [Glab s] -> getTextLabs acc0 ((s,size) : acc1) size xs
                                        _        -> getTextLabs (x : acc0) acc1 (size + 1) xs
  getTextLabs acc0 acc1 _    []     = (acc1, reverse acc0)

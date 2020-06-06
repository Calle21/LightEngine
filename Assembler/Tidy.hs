module Assembler.Tidy where

import Assembler.OpInfo (getOpSyntax)
import Types
import Ubi
import Util

tidy :: (FilePath, RegList, [[Token]], [[Token]]) -> (FilePath, [[Token]], [[Token]])
tidy (filename, regs, dat, text) = (filename, dat, rec text)
  where
  rec :: [[Token]] -> [[Token]]
  rec (x:xs) | isLabLine x = x : rec xs
             | otherwise   = let (Name s:args) = x
                                 syn           = fromJust $ getOpSyntax s
                             in (Name s : recArgs args syn) : rec xs
    where
    recArgs :: [Token] -> [Syn] -> [Token]
    recArgs xs (y:ys) = case y of
                          RG -> case xs of
                                  (Name s:xs') -> case s `lookup` regs of
                                                    Just i -> Reg i : recArgs xs' ys
                          IM -> head xs : recArgs (tail xs) ys
                          LB -> head xs : recArgs (tail xs) ys
                          IA -> case head xs of
                                  INum _ -> head xs : recArgs (tail xs) ys
                                  Name s -> Addr s : recArgs (tail xs) ys
                          PL -> case xs of
                                  (Name s:Plus:INum i:xs')  -> case s `lookup` regs of
                                                                 Just i0 -> Place i0 i : recArgs xs' ys
                                  (Name s:Minus:INum i:xs') -> case s `lookup` regs of
                                                                 Just i0 -> Place i0 (negate i) : recArgs xs' ys
                                  (Name s:xs')              -> case s `lookup` regs of
                                                                 Just i -> Place i 0 : recArgs xs' ys
                          SC -> case head xs of
                                  Name s -> case getSyscall s of
                                              Just i -> INum i : recArgs (tail xs) ys
    recArgs [] [] = []
  rec [] = []

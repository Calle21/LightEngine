module Assembler.Check where

import Assembler.OpInfo (getOpSyntax)
import Types
import Ubi
import Util

check :: (FilePath, Lexed) -> (FilePath, Checked)
check (filename, file) = (filename, ch 1 file)
  where
  ch :: Int -> Lexed -> Checked
  ch line (x:xs) | isDataLine x || isLabLine x || isInstrLine x = ch (line + 1) xs
    where
    isLabLine :: [Token] -> Bool
    isLabLine [x] = isLab x
    isLabLine _   = False
    isInstrLine :: [Token] -> Bool
    isInstrLine (Name s:xs) = case getOpSyntax s of
                                  Just syn -> rec xs syn
                                  Nothing  -> aerror filename line ("Not an opcode : " ++ s)
      where
      rec :: [Token] -> [Syn] -> Bool
      rec xs (y:ys) = let xs' = synMatch xs y
                      in case length ys of
                           0  -> rec xs' ys
                           1  -> case xs' of
                                   (Arrow:xs'') -> rec xs'' ys
                                   _            -> aerror filename line "Expected an arrow"
                           2  -> case xs' of
                                  (Comma:xs'') -> rec xs'' ys
                                  _            -> aerror filename line "Expected a comma"
        where
        synMatch :: [Token] -> Syn -> [Token]
        synMatch xs y = case y of
                          RG  -> case xs of
                                   (Reg i:xs')  -> if i <= 15 then xs'
                                                   else aerror filename line "Only 16 registers"
                                   _            -> aerror filename line "Expected a reg"
                          LB  -> case xs of
                                   (Name _:xs') -> xs'
                                   _            -> aerror filename line "Expected a labname"
                          IN  -> case xs of
                                   (INum _:xs') -> xs'
                                   _            -> aerror filename line "Expected an integer"
                          AD  -> case xs of
                                   (Name _:xs') -> case xs' of
                                                     (Plus:INum _:xs'')  -> xs''
                                                     (Minus:INum _:xs'') -> xs''
                                                     _                   -> xs'
                                   _            -> aerror filename line "Expected a labname for address"
      rec [] [] = True
      rec _  _  = aerror filename line "Too many / few tokens on line"
    isInstrLine (_:_) = aerror filename line "Expected an opcode"
  ch _ [] = file

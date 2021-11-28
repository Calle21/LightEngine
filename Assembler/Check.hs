module Assembler.Check (check) where

import Assembler.OpInfo
import Share
import Types
import Ubi

check :: (FilePath, [[Token]]) -> (FilePath, [[Token]])
check (filename, file) = (filename, ch 1 file)
  where
  ch :: Int -> [[Token]] -> [[Token]]
  ch line (x:xs) | isDataLine x || isLabLine x || isInstrLine x = ch (line + 1) xs
    where
    isInstrLine :: [Token] -> Bool
    isInstrLine (Name s:args) = case getOpSyntax s of
                                       Just syn -> rec args syn
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
                                   (Name s:xs') -> case s `lookup` regs of
                                                     Just _  -> xs'
                                                     Nothing -> error filename line ("That was not a name for any register : " ++ s)
                                   _            -> aerror filename line "Expected a name for a register"
                          LB  -> case xs of
                                   (Name _:xs') -> xs'
                                   _            -> aerror filename line "Expected a name for lab"
                          IN  -> case xs of
                                   (INum _:xs') -> xs'
                                   _            -> aerror filename line "Expected an integer"
                          IA  -> case xs of
                                   (INum i:xs') -> xs'
                                   (Name _:xs') -> xs'
                                   _            -> aerror filename line "Expected integer or name for li"
                          PL  -> case xs of
                                   (Name _:xs') -> case xs' of
                                                     (Plus:INum _:xs'')  -> xs''
                                                     (Minus:INum _:xs'') -> xs''
                                                     _                   -> xs'
                                   _            -> aerror filename line "Expected a name for a place"
                          SC  -> case xs of
                                   (Name s:xs') -> case getSyscall s of
                                                     Just _  -> xs'
                                                     Nothing -> aerror filename line ("That was not an argument for syscall : " ++ s)
                                   _            -> aerror filename line "Expected a name for a syscall"
      rec [] [] = True
      rec _  [] = aerror filename line "Too many tokens on line"
    isInstrLine (_:_) = aerror filename line "Expected an opcode"
  ch _ [] = file

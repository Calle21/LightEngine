module Types where

import Ubi

type ExeFunc = [Int64] -> RAM -> Proc -> IO Sig

type OpInfo = ([(String,ExeFunc)],[Syn],[Int64])

type Proc = RAM

type RegList = [(String,Int64)]

type RAM = Array Int64 (IORef Int64)

data Sig = Continue | Exit

type SymTable = [(String,Int64)]

data Syn = RG | IM | LB | PL | IA | SC

data Token = Addr String
           | Arrow
           | Comma
           | Glab String
           | INum Int64
           | Llab String
           | Minus
           | Name String
           | Place Int64 Int64
           | Plus
           | Reg Int64
           | Space Int64
           | Str String
           deriving (Eq, Show)

module Types where

import Ubi

type Checked = Lexed

type Clean = (Int64,[Int64],[[Int64]])

type Compact = (Int64, [Int64])

type Concat = Replaced

type ExeFunc = [Int64] -> RAM -> Proc -> IO Sig

type GotLabs = (SymTable,[Token],Checked)

type Lexed = [[Token]]

type Proc = RAM

type Replaced = Tidy

type RAM = Array Int64 (IORef Int64)

data Sig = Continue | Exit

type Split = (Checked,Checked)

type SymTable = [(String,Int64)]

data Syn = RG | IN | LB | AD

type Tidy = Split

data Token = Addr String Int64
           | Arrow
           | Comma
           | Glab String
           | INum Int64
           | Llab String
           | Minus
           | Name String
           | Plus
           | Reg Int64
           | Space Int64
           | Str String
           deriving (Eq, Show)















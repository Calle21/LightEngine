module MakeLines (makeLines,Token(..)) where

import Data.Char (isLower)
import Data.Word (Word16)
import Text.Regex.PCRE ((=~))

import Utilities (safeTail)

data ParenType = Open | Close

data Paren = Round | Square | Curly

data Number = Whole Word16
            | Float Word16

data Comparison = Eq | Less | Greater | EqLess | EqGreater | NotEq

data Instruction = B | 

data Token = Paren Paren ParenType
           | GLabel String [String] [String]
           | Label String
           | Number Number
           | Comma
           | If
           | RightArrow
           | Assignment
           | Comparison Comparison
           | Instruction Instruction
           | Name String

makeLines :: FilePath -> String -> [[Token]]
makeLines path contents = rec 1 contents
  where
  rec :: Int -> String -> [[Token]]
  rec _ [] = []
  rec ln s@(x:xs)
    | x == '\n' = rec (succ ln) xs
    | x == '@' = 
    | x == '\t'             = 
    | s =~ "^[a-zA-Z0-9_]" = 
    | s =~ "^$|^[ \\t]*;"  = 
  rec ln s = case lType s of
               En -> []
               Em -> rec $ succ ln $ tail $ dropWhile (/='\n') s
             else
    let (s',ln') = skipEmptyLines ln s

             rec $ safeTail $ dropWhile (/='\n') s

data LType = En | Em | Gl | La | Tb | Er

lType :: String -> LType
lType s = if head s == '\t' then Tb
           if s =~ "^$|^ ; .*" then if s =~ "\\n" then Em else En

isLine :: String -> Bool
isLine ('\t':c:_) = isLower c
isLine _ = False

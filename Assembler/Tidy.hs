module Assembler.Tidy where

import Types
import Ubi
import Util

tidy :: (FilePath, Split) -> (FilePath, Tidy)
tidy (filename, (dat,text)) = (filename, (dat,rec text))
  where
  rec :: Checked -> Checked
  rec (x:xs) | head x == Name "la" = (Name "li" : specialCase (tail x)) : rec xs
             | otherwise           = withoutIf punct x : rec xs
    where
    specialCase :: [Token] -> [Token]
    specialCase args = case args of
                         [Name s,_,c]              -> [Addr s 0, c]
                         [Name s,Plus,INum i,_,e]  -> [Addr s i, e]
                         [Name s,Minus,INum i,_,e] -> [Addr s (negate i), e]
  rec []     = []

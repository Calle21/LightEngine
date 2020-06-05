module Assembler.Replace where

import Types
import Ubi
import Util

replace :: (FilePath, Tidy) -> Replaced
replace (filename, (dat,text)) = let locDataLabs = getLocLabs dat
                                     locTextLabs = getLocLabs text
                                     locLabs     = locDataLabs ++ locTextLabs
                                     prefix      = takeBaseName filename ++ "-"
                                     dat'        = replaceData dat prefix
                                     text'       = replaceText text prefix locLabs
                                 in (dat',text')
  where
  getLocLabs :: Checked -> [String]
  getLocLabs ((Llab s:_):xs) = s : getLocLabs xs
  getLocLabs (_:xs)          = getLocLabs xs
  getLocLabs []              = []
  replaceData :: Checked -> String -> Checked
  replaceData ([Llab s,b]:xs) prefix = [Glab $ prefix ++ s, b] : replaceData xs prefix
  replaceData (x:xs)          prefix = x : replaceData xs prefix
  replaceData []              _      = []
  replaceText :: Checked -> String -> [String] -> Checked
  replaceText ([Llab s]:xs)  prefix locLabs = [Glab $ prefix ++ s] : replaceText xs prefix locLabs
  replaceText ((op:args):xs) prefix locLabs = case find isLabs args of
                                                Just _  -> (op : replaceArgs args) : replaceText xs prefix locLabs
                                                Nothing -> (op:args) : replaceText xs prefix locLabs
    where
    isLabs :: Token -> Bool
    isLabs (Name _)   = True
    isLabs (Addr _ _) = True
    isLabs _          = False
    replaceArgs :: [Token] -> [Token]
    replaceArgs (Name s:xs)   = if s `elem` locLabs
                                then Name (prefix ++ s) : replaceArgs xs
                                else Name s : replaceArgs xs
    replaceArgs (Addr s i:xs) = if s `elem` locLabs
                                then Addr (prefix ++ s) i : replaceArgs xs
                                else Addr s i : replaceArgs xs
    replaceArgs (x:xs)        = x : replaceArgs xs
    replaceArgs []            = []
  replaceText [] _ _ = []

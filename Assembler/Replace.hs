module Assembler.Replace where

import Types
import Ubi
import Util

replace :: (FilePath, [[Token]], [[Token]]) -> ([[Token]], [[Token]])
replace (filename, dat, text) = let locDataLabs = getLocLabs dat
                                    locTextLabs = getLocLabs text
                                    locLabs     = locDataLabs ++ locTextLabs
                                    prefix      = takeBaseName filename ++ "-"
                                    dat'        = replaceData dat prefix
                                    text'       = replaceText text prefix locLabs
                                in (dat',text')
  where
  getLocLabs :: [[Token]] -> [String]
  getLocLabs ((Llab s:_):xs) = s : getLocLabs xs
  getLocLabs (_:xs)          = getLocLabs xs
  getLocLabs []              = []
  replaceData :: [[Token]] -> String -> [[Token]]
  replaceData ([Llab s,d]:xs) prefix = [Glab $ prefix ++ s, d] : replaceData xs prefix
  replaceData (x:xs)          prefix = x : replaceData xs prefix
  replaceData []              _      = []
  replaceText :: [[Token]] -> String -> [String] -> [[Token]]
  replaceText ([Llab s]:xs)  prefix locLabs = [Glab $ prefix ++ s] : replaceText xs prefix locLabs
  replaceText ((op:args):xs) prefix locLabs = case find isLabs args of
                                                Just _  -> (op : replaceArgs args) : replaceText xs prefix locLabs
                                                Nothing -> (op:args) : replaceText xs prefix locLabs
    where
    isLabs :: Token -> Bool
    isLabs (Name _) = True
    isLabs (Addr _) = True
    isLabs _        = False
    replaceArgs :: [Token] -> [Token]
    replaceArgs (Name s:xs)   = if s `elem` locLabs
                                then Name (prefix ++ s) : replaceArgs xs
                                else Name s : replaceArgs xs
    replaceArgs (Addr s:xs) = if s `elem` locLabs
                              then Addr (prefix ++ s) : replaceArgs xs
                              else Addr s : replaceArgs xs
    replaceArgs (x:xs)        = x : replaceArgs xs
    replaceArgs []            = []
  replaceText [] _ _ = []

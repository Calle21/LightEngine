module Assembler.Lex (lex') where

import Types
import Ubi
import Util

lex' :: (FilePath, String) -> (FilePath, [[Token]])
lex' (filename,s) = (filename, lexLines 1 $ lines s)
  where
  lexLines :: Int -> [String] -> [[Token]]
  lexLines line (x:xs) = case lexLine x of
                           []  -> lexLines (line + 1) xs
                           xs' -> xs' : lexLines (line + 1) xs
    where
    lexLine :: String -> [Token]
    lexLine s = case dropWhile ws s of
                  []     -> []
                  (x:xs) -> case x of
                              ',' -> Comma : lexLine xs
                              ';' -> []
                              '"' -> let (st,xs') = getString [] xs
                                     in Str st : lexLine xs'
                              _   -> if nonPunct x
                                     then let (ts,xs') = span nonPunct (x:xs)
                                              tok | arrowSyntax ts   = Arrow
                                                  | glabSyntax ts    = Glab $ tail ts
                                                  | inumSyntax ts    = INum $ read ts
                                                  | llabSyntax ts    = Llab $ init ts
                                                  | minusSyntax ts   = Minus
                                                  | nameSyntax ts    = Name ts
                                                  | plusSyntax ts    = Plus
                                                  | spaceSyntax ts   = Space $ read $ init $ tail ts
                                                  | otherwise        = aerror filename line ("Bad token : " ++ ts)
                                          in tok : lexLine xs'
                                     else aerror filename line ("Illegal character : " ++ [x])
      where
      getString :: String -> String -> (String, String)
      getString acc (x:xs) | x == '\\' = case xs of
                                           ('\\':xs') -> getString ('\\' : acc) xs'
                                           ('"':xs')  -> getString ('"' : acc) xs'
                                           ('n':xs')  -> getString ('\n' : acc) xs'
                                           ('t':xs')  -> getString ('\t' : acc) xs'
                                           (c:_)      -> aerror filename line ("Illegal escape char : " ++ [c])
                                           []         -> aerror filename line "Line ended in middle of escape char"
                           | x == '"'  = (reverse $ '\0' : acc,xs)
                           | otherwise = getString (x : acc) xs
      getString _   []     = aerror filename line "Couldn't find end of string"
  lexLines _ [] = []
      
nonPunct :: Char -> Bool
nonPunct c = labChar c || c == '@' || c == ':' || c == '|' || c == '$' || c == '-' || c == '>' || c == '+'

ws :: Char -> Bool
ws c = c == ' ' || c == '\t'

labChar :: Char -> Bool
labChar c | c >= 'a' && c <= 'z' = True
          | c >= 'A' && c <= 'Z' = True
          | c >= '0' && c <= '9' = True
          | c == '_'             = True
          | otherwise            = False

arrowSyntax s = s == "->"

glabSyntax s = not (null s) && nameSyntax (tail s) && head s == '@'

inumSyntax s = not (null s) && unsignedSyntax (if head s == '-' then tail s else s)

minusSyntax s = s == "-"

nameSyntax s = not (null s) && all labChar s

llabSyntax s = not (null s) && nameSyntax (init s) && last s == ':'

plusSyntax s = s == "+"

spaceSyntax s = not (null s) && head s == '|' && last s == '|' && unsignedSyntax (init $ tail s)

unsignedSyntax s = not (null s) && all (\c -> c >= '0' && c <= '9') s

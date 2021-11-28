module Assembler.Lex (lex') where

import Share
import Types
import Ubi

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
                                              tok | arrowSyntax ts = Arrow
                                                  | fnumSyntax ts  = FNum $ read ts
                                                  | glabSyntax ts  = Glab $ tail ts
                                                  | inumSyntax ts  = INum $ read ts
                                                  | llabSyntax ts  = Llab $ init ts
                                                  | minusSyntax ts = Minus
                                                  | nameSyntax ts  = Name ts
                                                  | plusSyntax ts  = Plus
                                                  | spaceSyntax ts = Space $ read $ init $ tail ts
                                                  | otherwise      = aerror filename line ("Bad token : " ++ ts)
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
nonPunct c = labChar c || c == '@' || c == ':' || c == '|' || c == '$' || c == '-' || c == '>' || c == '+' || c == '.'

ws :: Char -> Bool
ws c = c == ' ' || c == '\t'

labChar :: Char -> Bool
labChar c | c >= 'a' && c <= 'z' = True
          | c >= 'A' && c <= 'Z' = True
          | c >= '0' && c <= '9' = True
          | c == '_'             = True
          | otherwise            = False

arrowSyntax, fnumSyntax,
 glabSyntax, inumSyntax,
  llabSyntax, minusSyntax,
   nameSyntax, plusSyntax,
    spaceSyntax :: String -> Bool

arrowSyntax s = s == "->"

fnumSyntax s = s =~ "^-?[0-9]+\\.[0-9]+$"

glabSyntax s = s =~ "^@[0-9a-zA-Z_]+$"

inumSyntax s = s =~ "^-?[0-9]+$"

llabSyntax s = s =~ "^[a-zA-Z0-9_]+:$"

minusSyntax s = s == "-"

nameSyntax s = s =~ "^[a-zA-Z_][a-zA-Z0-9]*$"

plusSyntax s = s == "+"

spaceSyntax s = s =~ "^\\|[0-9]+\\|$"

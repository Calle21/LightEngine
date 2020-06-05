module Assembler.Lex where

import Types
import Ubi
import Util

lex' :: (FilePath, String) -> (FilePath, Lexed)
lex' (filename,s) = (filename, withoutIf null $ lexLines 1 $ lines s)
  where
  lexLines :: Int -> [String] -> Lexed
  lexLines _    []     = []
  lexLines line (x:xs) = lexLine x : lexLines (line + 1) xs
    where
    lexLine :: String -> [Token]
    lexLine s = let s' = dropWhile ws s
                in if null s' then []
                   else case head s' of
                          ','  -> Comma : lexLine (tail s')
                          ';'  -> []
                          '"'  -> let (st,s'') = getString [] (tail s')
                                  in Str st : lexLine s''
                          c    -> if nonPunct c
                                  then let (ts,s'') = span nonPunct s'
                                           tok | arrowSyntax ts   = Arrow
                                               | glabSyntax ts    = Glab $ tail ts
                                               | inumSyntax ts    = INum $ read ts
                                               | llabSyntax ts    = Llab $ init ts
                                               | minusSyntax ts   = Minus
                                               | nameSyntax ts    = Name ts
                                               | plusSyntax ts    = Plus
                                               | regSyntax ts     = Reg $ read (tail ts)
                                               | spaceSyntax ts   = Space $ read $ init $ tail ts
                                               | otherwise        = aerror filename line ("Bad token : " ++ ts)
                                       in tok : lexLine s''
                                  else aerror filename line ("Illegal character : " ++ [c])
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

regSyntax s = not (null s) && head s == '$' && unsignedSyntax (tail s)

spaceSyntax s = not (null s) && head s == '|' && last s == '|' && unsignedSyntax (init $ tail s)

unsignedSyntax s = not (null s) && all (\c -> c >= '0' && c <= '9') s

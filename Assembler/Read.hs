module Assembler.Read where

import qualified Data.ByteString.Char8 as C
import Ubi
import Util

data Token = Branch Int32
           | Comma
           | Comment
           | Label Int32
           | Number Int Int32
           | Op Int32
           | Par

read' :: [(C.ByteString,Int)] -> C.ByteString -> [Int32]
read' symbolTable = loop 1
  where
  loop :: Int -> C.ByteString -> [Int32]
  loop ln s = let (s',dropped) = dropEmptyLines 0 s
              in if C.null s' then []
                 let (toks,s'') = getALine [] s'
                     toks'      = checkPar toks
                 in concatenateToks toks' : loop (ln + 1 + dropped) s''
    where
    checkPar :: [Token] -> [Token]
    checkPar (Par:xs) = Number 1 1 : checkOp xs
    checkPar xs       = Number 0 1 : checkOp xs
      where
      checkOp :: [Token] -> [Token]
      checkOp [Label addr] = [Number (fromJust $ opcode (C.pack "call")) 5, Number addr 26]
      checkOp (Op n:xs)    = Number n 5 : checkArgs xs
      checkOp _            = readError ln (C.pack "Expected lone label or op at beginning of line")
        where
        checkArgs :: [Token] -> [Token]
        checkArgs (x:Comma:xs) = x : checkArgs xs
        checkArgs (_:_:_)      = readError ln (C.pack "Expected a comma")
        checkArgs [Comma]      = readError ln (C.pack "Comma at end of line")
        checkArgs [x]          = [x]
        checkArgs []           = []
    concatenateToks :: [Token] -> Int32
    concatenateToks toks = let raw = map toNumber toks
                           in if sumOn (\Number _ size -> size) raw == 32
                              then foldl' (\prev (Num n s) -> prev `shiftL` s .|. n) 0 raw
                              else readError ln (C.pack "Instruction didn't sum to 32 bits size")
      where
      toNumber :: Token -> Token
      toNumber (Branch n)   = Num n 3
      toNumber n@(Num _ _ ) = n
      toNumber _            = readError ln (C.pack "Bad token (expected branch type or num)")
    dropEmptyLines :: Int -> C.ByteString -> (C.ByteString, Int)
    dropEmptyLines dropped s = let (t,s') = getNextTokenOnLine s
                               in case t of
                                    Nothing -> dropEmptyLines (dropped + 1) s'
                                    Just _  -> (s,dropped)
    getALine :: [Token] -> Int -> C.ByteString -> ([Token],C.ByteString)
    getALine acc s = case getNextTokenOnLine s of
                       (Nothing,s') -> (reverse acc, s')
                       (Just t, s') -> getALine (t : acc) s'
    getNextTokenOnLine :: C.ByteString -> (Maybe Token,C.ByteString)
    getNextTokenOnLine s = let s' = C.dropWhile whiteSpace s
                           in if C.null s' || C.head s' == '\n' then (Nothing,safeTail' s')
                              else if C.head s' == ',' then (Just Comma, C.tail s')
                              else let (ts,s'') = C.span tokChar s'
                                       t | branchSyntax ts  = Branch $ fromJust (branch ts)
                                         | commentSyntax ts = Comment
                                         | labelSyntax  ts  = case ts `lookup` symbolTable of
                                                                Nothing   -> readError ln (C.pack "No function with that name")
                                                                Just addr -> Label addr
                                         | numberSyntax ts  = let (num,ts') = case C.readInt ts of
                                                                                Nothing -> (0,ts)
                                                                                j       -> fromJust j
                                                                  (size,_)  = fromJust (C.readInt (C.tail ts'))
                                                              in Number num size
                                         | opSyntax ts      = Op $ fromJust (opcode ts)
                                         | parSyntax ts     = Par
                                         | otherwise        = readError ln (C.pack "Bad token")
                                   in case t of
                                        Comment -> (Nothing, C.tail (C.dropWhile (/='\n') s''))
                                        t'      -> (Just t', s'')

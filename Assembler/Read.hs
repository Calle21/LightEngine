module Assembler.Read where

import qualified Data.ByteString.Char8 as C
import Ubi
import Util

data Token = Branch Int64
           | Comma
           | Comment
           | Firm Int64
           | Label Int64
           | Minus
           | Number Int64 Int
           | Offset Int64
           | Op Int64
           | Plus

read' :: String -> C.ByteString -> [Int64]
read' filename = loop 1
  where
  loop :: Int -> C.ByteString -> [Int64]
  loop ln s = let (s',dropped, t) = dropEmptyLines 0 s
              in case t of
                   Nothing -> []
                   Just t  -> let (toks,s'') = getALine [t] s'
                                  toks'      = checkOp toks
                              in concatenateToks toks' : loop (ln + dropped) s''
    where
    checkOp :: [Token] -> [Token]
    checkOp [Label addr] = [Number (fromJust $ opcode (C.pack "call")) 5, Number addr 26]
    checkOp (Op n:xs)    = Number n 6 : checkArgs xs
    checkOp _            = readError filename ln (C.pack "Expected lone label or op at beginning of line")
      where
      checkArgs :: [Token] -> [Token]
      checkArgs (x:Comma:xs)                   = x : checkArgs xs
      checkArgs (Label addr:Plus:Offset i:xs)  = checkArgs (Number (addr + i) 20 : xs)
      checkArgs (Label addr:Minus:Offset i:xs) = checkArgs (Number (addr - i) 20 : xs)
      checkArgs (_:_:_)                        = readError filename ln (C.pack "Bad syntax")
      checkArgs [Comma]                        = readError filename ln (C.pack "Comma at end of line")
      checkArgs [x]                            = [x]
      checkArgs []                             = []
    concatenateToks :: [Token] -> Int64
    concatenateToks toks = let raw = map toNumber toks
                           in if sumOn (\Number _ size -> size) raw == 32
                              then foldl' (\prev (Number n s) -> prev `shiftL` s .|. n) 0 raw
                              else readError filename ln (C.pack "Instruction didn't sum to 32 bits size")
      where
      toNumber :: Token -> Token
      toNumber (Branch n)      = Number n 4
      toNumber n@(Number _ _ ) = n
      toNumber (Label addr)    = Number addr 20
      toNumber (Firm n)        = Number n 4
      toNumber _               = readError filename ln (C.pack "Bad token (expected branch type, num, label or firmware code)")
    dropEmptyLines :: Int -> C.ByteString -> (C.ByteString, Int, Maybe Token)
    dropEmptyLines dropped s | C.null s  = (s, dropped, Nothing)
                             | otherwise = let (t,s') = getNextTokenOnLine s
                                           in case t of
                                                Nothing -> dropEmptyLines (dropped + 1) s'
                                                t       -> (s',dropped,t)
    getALine :: [Token] -> C.ByteString -> ([Token],C.ByteString)
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
                                         | firmSyntax ts    = Firm $ fromJust (firmCode ts)
                                         | labelSyntax  ts  = case ts `lookup` symbolTable of
                                                                Nothing   -> readError filename ln (C.pack "That label not in symbol table")
                                                                Just addr -> Label addr
                                         | minusSyntax ts   = Minus
                                         | numberSyntax ts  = let (num,ts') = case C.readInt ts of
                                                                                Nothing -> (0,ts)
                                                                                j       -> fromJust j
                                                                  (size,_)  = fromJust (C.readInt (C.tail ts'))
                                                              in Number (fromIntegral num) size
                                         | offsetSyntax ts  = fromIntegral $ fst $ fromJust $ C.readInt ts
                                         | opSyntax ts      = Op $ fromJust (opcode ts)
                                         | plusSyntax ts    = Plus
                                         | otherwise        = readError filename ln (C.pack "Bad token")
                                   in case t of
                                        Comment -> (Nothing, C.tail (C.dropWhile (/='\n') s''))
                                        t'      -> (Just t', s'')
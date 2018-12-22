module Assembler.Syntax where

import qualified Data.ByteString.Char8 as C
import Text.Regex.PCRE((=~))
import Ubi

 -- Char

tokChar :: Char -> Bool
tokChar c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c == '_' || c == '[' || c == ']' || c == '-' || c == '+'

 -- Reserved

branch, opcode :: C.ByteString -> Maybe Int

branch s = s `elemIndex'` branches

branches = listArray (0,15) (map C.pack ["al"
                                       , "eq"
                                       , "eqi"
                                       , "ez"
                                       , "ge"
                                       , "gef"
                                       , "gei"
                                       , "geu"
                                       , "gt"
                                       , "gtf"
                                       , "gti"
                                       , "gtu"
                                       , "gtz"
                                       , "lei"
                                       , "lti"
                                       , "ltz"]

firmCode s = s `elemIndex'` firmCodes

firmCodes = listArray (0,31) (map C.pack ["putChar"
                                        , "putInt"
                                        , "putFloat"
                                        , "putAddress"]

opcode s = s `elemIndex'` opcodes

opcodes = listArray (0,63) (map C.pack ["add"
                                      , "addi"
                                      , "and"
                                      , "branch"
                                      , "call"
                                      , "cmpjmp"
                                      , "decr"
                                      , "firm"
                                      , "incr"
                                      , "li"
                                      , "lw"
                                      , "lwi"
                                      , "move"
                                      , "mul"
                                      , "not"
                                      , "or"
                                      , "resetparams"
                                      , "return"
                                      , "setparams"
                                      , "sub"
                                      , "subi"
                                      , "sw"
                                      , "swi"
                                      , "xor"]

reserved s = branchSyntax s || opSyntax s

 -- Token syntax

branchSyntax, commentSyntax, firmSyntax, labelSyntax, minusSyntax,
  numberSyntax, offsetSyntax, opSyntax, plusSyntax :: C.ByteString -> Bool

branchSyntax s = isJust $ s `elemIndex'` branches

commentSyntax s = s == C.pack "--"

firmSyntax s = isJust $ s `elemIndex'` firmCodes

labelSyntax s = not reserved s && C.unpack s =~ "^[a-zA-Z_][a-zA-Z_0-9]*$"

minusSyntax s = s == C.pack "-"

numberSyntax s = C.unpack s =~ "^(-?[0-9]+)?\\[[0-9]+\\]$"

offsetSyntax s = C.unpack s =~ "^[0-9]+$"

opSyntax s = isJust $ s `elemIndex'` opcodes

plusSyntax s = s == (C.pack "+")

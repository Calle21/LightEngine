module Assembler.Syntax where

import Text.Regex.PCRE((=~))
import Ubi
import Util

branch :: C.ByteString -> Maybe Int
branch s = s `elemIndex'` branches

branches = listArray (0,7) (map C.pack ["al"
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

branchSyntax, commentSyntax, minusSyntax
  numberSyntax, labelSyntax, offsetSyntax
    opSyntax, plusSyntax :: C.ByteString -> Bool

branchSyntax s = isJust $ s `elemIndex'` branches

commentSyntax s = s == C.pack "--"

labelSyntax = not . branchSyntax &&& not . opSyntax &&& not . parSyntax &&& C.all isLabChar

minusSyntax s = s == C.pack "-"

numberSyntax s = C.unpack s =~ "^(-?[0-9]+)?\\[[0-9]+\\]$"

offsetSyntax s = C.unpack s =~ "^[0-9]+$"

opcode :: C.ByteString -> Maybe Int
opcode s = s `elemIndex'` opcodes

opcodes = listArray (0,63) (map C.pack ["add"
                                      , "addi"
                                      , "and"
                                      , "branch"
                                      , "call"
                                      , "cmpjmp"
                                      , "decr"
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
                                      , "wait"
                                      , "xor"]

opSyntax s = isJust $ s `elemIndex'` opcodes

plusSyntax s = s == (C.pack "+")

 -- punctChar c = c == ',' || c == '.'

tokChar c = isLabChar c || c == '[' || c == ']' || c == '-' || c == '+'

whiteSpace c = c == ' ' || c == '\t'

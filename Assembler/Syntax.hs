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
                                      , "gei"
                                      , "gt"
                                      , "gti"]

branchSyntax, commentSyntax, numberSyntax, labelSyntax, opSyntax, parSyntax :: C.ByteString -> Bool

branchSyntax s = isJust $ s `elemIndex'` branches

commentSyntax s = s == C.pack "--"

labelSyntax = not . branchSyntax &&& not . opSyntax &&& not . parSyntax &&& C.all isLabChar

numberSyntax s = C.unpack s =~ "^(-?[0-9]+)?\\[[0-9]+\\]$"

opcode :: C.ByteString -> Maybe Int
opcode s = s `elemIndex'` opcodes

opcodes = listArray (0,63) (map C.pack ["add"
                                      , "addi"
                                      , "and"
                                      , "branch"
                                      , "call"
                                      , "callpar"
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

parSyntax s = s == (C.pack "par")

punctChar c = c == ','

tokChar c = isLabChar c || c == '[' || c == ']' || c == '-'

whiteSpace c = c == ' ' || c == '\t'

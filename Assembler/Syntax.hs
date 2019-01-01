module Assembler.Syntax where

import qualified Data.ByteString.Char8 as C
import Text.Regex.PCRE((=~))
import Ubi

 -- Char

tokChar :: Char -> Bool
tokChar c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c == '_' || c == '[' || c == ']' || c == '-' || c == '+'

 -- Op syntax

opSyntax = packIt [(["abs","fact","not","move", "swap"]
                    , place ++ place)
                 , (["acos","add","asin","atan","cos","div","exp","log","max","min","mul","sin","sub","tan"]
                    , concat $ replicate 3 tplace)
                 , (["addi","andi","divi","expi","maxi","mini","muli","ori","xori"]
                    , place ++ simm ++ place)
                 , (["and","or","rl","rr","sl","sr","sra","xor"]
                    , concat $ replicate 3 place)
                 , (["branch"], [1,3]) -- Plus special
                 , (["call", "la"], [58])
                 , (["clearbit","setbit","togglebit"]
                    , place ++ bit ++ place)
                 , (["convert"], tplace ++ tplace)
                 , (["device"], dtype ++ replicate 6 place)
                 , (["return","wait"], place)
                 , (["rli","rri","sli","sri","srai"]
                    , place ++ shift ++ place)]
  where
  packIt :: [([String], [Int])] -> Array Int (Array Int C.ByteString, Array Int Int)
  packIt xs = listToArray (map packTuple xs)
    where
    packTuple (a,b) = (listToArray (map C.pack a), listToArray b)
  bit, dtype, imm, jump, place, shift, simm, tplace, vtype :: [Int]
  bit    = [6]
  dtype  = [10]
  imm    = [16]
  jump   = [16]
  place  = [4,1,4]
  shift  = [6]
  simm   = [6,16]
  tplace = [2,4,1,4]
  vtype  = [2]

 -- Reserved

branch, opcode :: C.ByteString -> Maybe Int

branch s = s `elemIndex'` branches

branches = listToArray (map C.pack ["al" , "bit" , "eq" , "eqi"
                                  , "gt" , "gti" , "lt" , "lti"]

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

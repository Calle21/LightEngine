module Operations (getOp) where

import qualified Data.ByteString.Char8 as C
import Types
import Ubi
import Util

getOp :: C.ByteString -> Operation
getOp s = fromJust $ binSearch s operations

operations :: Array Int (C.ByteString, Operation)
operations = listToArray [
                          (C.pack "add", addmul (+))
                        , (C.pack "and", std2op (.&.))
                        , (C.pack "div", \proc _  _ ->
                            let (ris, arg')   = decodes 4 3 (readReg (proc ! 6))
                                [r0,r1,d0,d1] = map (proc !) ris
                                [r0', r1']    = map readReg [r0,r1]
                                (d0', d1')    = divMod reg0' reg1'
                            in do writeReg (proc ! 6) arg'
                                  writeReg d1 d1'
                                  writeReg d0 d0'
                        , (C.pack "mov", std1op id)
                        , (C.pack "mul", addmul (*))
                        , (C.pack "not", std1op complement)
                        , (C.pack "or",  std2op (.|.))
                        , (C.pack "sl",  std2op shiftL)
                        , (C.pack "srl", std2op shiftR)
                        , (C.pack "sra", std2op shiftR)
                        , (C.pack "sub", std2op (-))
                        , (C.pack "xor", std2op xor)
                        ]
  where
  addmul :: (Word64 -> Word64 -> Word64) -> Operation
  addmul op proc _ _ = let (ris,arg')    = decodes 4 3 (readReg (proc ! 6))
                           [r0,r1,d0,d1] = map (proc !) ris

  std1op :: (Word64 -> Word64) -> Operation
  std1op op proc _ _ = let (ris,arg') = decodes 2 3 (readReg (proc ! 6))
                           [reg,des]  = map (proc !) ris
                           des'       = op (readReg reg)
                       in do writeReg (proc ! 6) arg'
                             writeReg des des'
  std2op :: (Word64 -> Word64 -> Word64) -> Operation
  std2op op proc _ _ = let (ris,arg')  = decodes 3 3 (readReg (proc ! 6))
                           [r0,r1,des] = map (proc !) ris
                           [r0',r1']   = map readReg [r0,r1]
                           des'        = r0' `op` r1'
                       in do writeReg (proc ! 6) arg'
                             writeReg des des'

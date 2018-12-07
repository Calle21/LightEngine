module Operations (getOp) where

import qualified Data.ByteString.Char8 as C
import Types
import Ubi
import Util

getOp :: C.ByteString -> Operation
getOp s = fromJust $ binSearch s operations

nextInstr -> Processor -> RAM -> IO ()
nextInstr proc ram = do ic <- readReg (proc ! 7)
                        let phase    = ic `shiftR` 18 .&. mask 2
                            newphase = ((phase + 1) `mod` 4) `shiftL` 18
                        if phase == 0 then do let addr = ic `mod` bit 22
                                              w <- readReg (ram ! addr)
                                              writeReg (proc ! 6) w
                                      else do fet <- readReg (proc ! 6)
                                              writeReg (proc ! 6) (fet `shiftR` 16)
                        let ic' = clearBit (clearBit (ic + 1) 18) 19 .|. newphase
                        writeReg (proc ! 7) ic'

operations :: Array Int (C.ByteString, Operation)
operations = listToArray [
                          (C.pack "add", addsub (+))
                        , (C.pack "and", std2op (.&.))
                        , (C.pack "cmp", compares id)
                        , (C.pack "cmps", compares )
                        , (C.pack "exit", )
                        , (C.pack "frk", )
                        , (C.pack "if", \(proc,_) ram ->
                             do arg <- readReg (proc ! 6)
                                let (cond,arg') = decode 2 arg
                                    (amount,_)  = decode 2 arg'
                                cond' <- readReg (proc ! 4)
                                if cond == cond' then return ()
                                                 else replicateM_ amount (nextInstr proc ram))
                        , (C.pack "io", )
                        , (C.pack "li", \(proc,_) _ ->
                             do arg <- readReg (proc ! 6)
                                let (desi,arg') = decode 3 arg
                                    (imm,_)     = decode 8 arg'
                                    sign        = testBit imm 7
                                    imm'        = bitSet (clearBit imm 7) 19 sign
                                writeReg (proc ! desi) imm'
                        , (C.pack "lw", swlw id)
                        , (C.pack "mov", std1op id)
                        , (C.pack "not", std1op complement)
                        , (C.pack "or",  std2op (.|.))
                        , (C.pack "sl",  std2op shiftL)
                        , (C.pack "sra", std2op shiftR)
                        , (C.pack "srl", std2op shiftR)
                        , (C.pack "sub", addsub (-))
                        , (C.pack "sw", swlw swap)
                        , (C.pack "xor", std2op xor)
                        ]
  where
  addsub :: (Word32 -> Word32 -> Word32) -> Operation
  addsub op proc _ = do arg <- readReg (proc ! 6)
                        let (ris,_)     = decodes 3 3 arg
                            [r0,r1,des] = map (proc !) ris
                        [r0',r1'] <- mapM readReg [r0,r1]
                        let (sc,des')   = r0' `op` r1' `divMod` 2 ^ 20
                        writeReg (proc ! 4) sc
                        writeReg des des'
  compares :: Integral a, Integral b => (a -> b) -> Operation
  compares conv proc _ = do arg <- readReg (proc ! 6)
                            let (ris,_) = decodes 2 3 arg
                                regs    = map (proc !) ris
                            regs' <- mapM readReg regs
                            let [r0,r1] = map conv regs'
                            writeReg (proc ! 4) fromEnum (compare r0 r1)
  std1op :: (Word32 -> Word32) -> Operation
  std1op op proc _ = do arg <- readReg (proc ! 6)
                        let (ris,_)   = decodes 2 3 arg
                            [reg,des] = map (proc !) ris
                        reg' <- readReg reg
                        writeReg des (op reg')
  std2op :: (Word32 -> Word32 -> Word32) -> Operation
  std2op op proc _ = do arg <- readReg (proc ! 6)
                        let (ris,_)     = decodes 3 3 arg
                            [r0,r1,des] = map (proc !) ris
                        [r0',r1'] <- map readReg [r0,r1]
                        writeReg des (r0' `op` r1')
  swlw :: (a -> b) -> Operation
  swlw sw proc ram = do arg <- readReg (proc ! 6)
                        let (ris,arg') = decodes 2 4 arg
                            (imm,_)    = decode 7 arg'
                            [r0,r1]    = map (proc !) ris
                        ramr <- (\i -> ram ! (i + imm)) `fmap` readReg r1
                        let (to,from) = sw (r0,ramr)
                        v <- readReg from
                        writeReg to v

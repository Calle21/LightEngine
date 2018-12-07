module Operations (getOp) where

import qualified Data.ByteString.Char8 as C
import Types
import Ubi
import Util

getOp :: C.ByteString -> Operation
getOp s = fromJust $ binSearch s operations

nextInstr -> Processor -> RAM -> IO ()
nextInstr proc ram = do ic <- readReg (proc ! 7)
                        let phase    = ic `shiftR` 62 .&. mask 2
                            newphase = ((phase + 1) `mod` 4) `shiftL` 62
                        if phase == 0 then do let addr = ic `mod` bit 22
                                              w <- readReg (ram ! addr)
                                              writeReg (proc ! 6) w
                                      else do fet <- readReg (proc ! 6)
                                              writeReg (proc ! 6) (fet `shiftR` 16)
                        let ic' = clearBit (clearBit (ic + 1) 62) 63 .|. newphase
                        writeReg (proc ! 7) ic'

operations :: Array Int (C.ByteString, Operation)
operations = listToArray [
                          (C.pack "add", addsub (+))
                        , (C.pack "addi", addsubi (+))
                        , (C.pack "and", std2op (.&.))
                        , (C.pack "bit", \proc _ _ ->
                             do arg <- readReg (proc ! 6)
                                let (regi, arg') = decode 3 arg
                                    reg          = proc ! regi
                                    (bit,_)      = decode 6 arg')
                                val <- readIORef (reg ! bit)
                                writeIORef (reg ! bit) (not val)
                        , (C.pack "cmp", compares id)
                        , (C.pack "cmps", compares (fromIntegral :: Word64 -> Int64))
                        , (C.pack "decode", \proc _ _ ->
                             do arg <- readReg (proc ! 6)
                                let (ris,arg') = decodes 2 3 arg
                                    (amount,_) = decode 5 arg'
                                    [reg,des]  = map (proc !) ris
                                regv <- readReg reg
                                let (code,new) = decode amount regv
                                writeReg reg new
                                writeReg des code)
                        , (C.pack "div", muldiv divMod)
                        , (C.pack "exit", )
                        , (C.pack "fadd", )
                        , (C.pack "fdiv", )
                        , (C.pack "fmul", )
                        , (C.pack "frk", )
                        , (C.pack "fsub", )
                        , (C.pack "if", \proc _ ram ->
                             do arg <- readReg (proc ! 6)
                                let (cond,arg') = decode 2 arg
                                    (amount,_)  = decode 2 arg'
                                cond' <- readReg (proc ! 4)
                                if cond == cond' then return ()
                                                 else replicateM_ amount (nextInstr proc ram))
                        , (C.pack "io", )
                        , (C.pack "li", \proc _ _ ->
                             do arg <- readReg (proc ! 6)
                                let (desi,arg') = decode 3 arg
                                    (imm,_)     = decode 8 arg'
                                    sign        = testBit imm 7
                                    imm'        = bitSet (clearBit imm 7) 63 sign
                                writeReg (proc ! desi) imm'
                        , (C.pack "lw", swlw id)
                        , (C.pack "mask", \proc _ _ ->
                             do arg <- readReg (proc ! 6)
                                let (imm,arg') = decode 6 arg
                                    (des,_)    = decode 3 arg'
                                    mask       = maxBound `shiftL` (64 - imm - 1)
                                writeReg (proc ! des) mask)
                        , (C.pack "mff", mffmtf id)
                        , (C.pack "mov", std1op id)
                        , (C.pack "mtf", mffmtf swap)
                        , (C.pack "mul", muldiv \i0 i1 -> swap $ (i0 * i1) `divMod` (2 ^ 64))
                        , (C.pack "not", std1op complement)
                        , (C.pack "or",  std2op (.|.))
                        , (C.pack "sl",  std2op shiftL)
                        , (C.pack "sra", std2op shiftR)
                        , (C.pack "srl", std2op shiftR)
                        , (C.pack "sub", addsub (-))
                        , (C.pack "subi", addsubi (-))
                        , (C.pack "sw", swlw swap)
                        , (C.pack "xor", std2op xor)
                        ]
  where
  addsub :: (Integer -> Integer -> Integer) -> Operation
  addsub op proc _ _ = do arg <- readReg (proc ! 6)
                          let (ris,_)     = decodes 3 3 arg
                              [r0,r1,des] = map (proc !) ris
                          [r0',r1'] <- mapM readReg [r0,r1]
                          let (sc,des')   = fromIntegral r0' `op` fromIntegral r1' `divMod` 2 ^ 64 :: (Integer,Integer)
                          writeReg (proc ! 4) (fromIntegral sc)
                          writeReg des (fromIntegral des')
  addsubi :: (Word64 -> Word64 -> Word64) -> Operation
  addsubi op proc _ _ = do arg <- readReg (proc ! 6)
                           let (r0i,arg')     = decode 3 (readReg (proc ! 6))
                               (amount,arg'') = decode 5 arg'
                               (desi,_)       = decode 3 arg''
                           [r0',r1'] <- mapM readReg (map (proc !) [r0i,r1i])
                           let des' = r0' `op` r1'
                           writeReg (proc ! desi) des'
  compares :: Integral a, Integral b => (a -> b) -> Operation
  compares conv proc _ _ = do arg <- readReg (proc ! 6)
                              let (ris,_) = decodes 2 3 arg
                                  regs    = map (proc !) ris
                              regs' <- mapM readReg regs
                              let [r0,r1] = map conv regs'
                              writeReg (proc ! 4) fromEnum (compare r0 r1)
  mffmtf :: (a -> b) -> Operation
  mffmtf sw proc coproc _ = do arg <- readReg (proc ! 6)
                               let (fregi,_) = decode 3 arg
                                   freg      = coproc ! fregi
                                   scr       = proc ! 4
                                   (to,from) = sw (scr,freg)
                               writeReg to =<< readReg from
  muldiv :: (Integer -> Integer -> (Integer,Integer)) -> Operation
  muldiv op proc _ _ = do arg <- readReg (proc ! 6)
                          let (r0i,arg')  = decode 3 arg
                              (r1i,arg'') = decode 2 arg'
                              ([d0,d1],_) = decodes 2 3 arg''
                          [r0',r1'] <- map readReg (map (proc !) [r0i,r1i])
                          let (d0',d1') = fromIntegral r0' `op` fromIntegral r1'
                          writeReg d1 (fromIntegral d1')
                          writeReg d0 (fromIntegral d0')
  std1op :: (Word64 -> Word64) -> Operation
  std1op op proc _ _ = do arg <- readReg (proc ! 6)
                          let (ris,_)   = decodes 2 3 arg
                              [reg,des] = map (proc !) ris
                          reg' <- readReg reg
                          writeReg des (op reg')
  std2op :: (Word64 -> Word64 -> Word64) -> Operation
  std2op op proc _ _ = do arg <- readReg (proc ! 6)
                          let (ris,_)     = decodes 3 3 arg
                              [r0,r1,des] = map (proc !) ris
                          [r0',r1'] <- map readReg [r0,r1]
                          writeReg des (r0' `op` r1')
  swlw :: (a -> b) -> Operation
  swlw sw proc _ ram = do arg <- readReg (proc ! 6)
                          let (ris,arg') = decodes 2 3 arg
                              (imm,_)    = decode 5 arg'
                              [r0,r1]    = map (proc !) ris
                          ramr <- (\i -> ram ! (i + imm)) `fmap` readReg r1
                          let (to,from) = sw (r0,ramr)
                          v <- readReg from
                          writeReg to v

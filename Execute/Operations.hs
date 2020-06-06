module Execute.Operations where

import Types
import Ubi
import Util

regRegReg, regImmDes :: (Int64 -> Int64 -> Int64) -> ExeFunc

regRegReg fn [r0,r1,des] _ proc = do r0' <- readIORef (proc ! r0)
                                     r1' <- readIORef (proc ! r1)
                                     let des' = r0' `fn` r1'
                                     writeIORef (proc ! des) des'
                                     return Continue

regImmDes fn [r0,imm,des] _ proc = do r0'  <- readIORef (proc ! r0)
                                      let des' = r0' `fn` imm
                                      writeIORef (proc ! des) des'
                                      return Continue

regReg :: (Int64 -> Int64) -> ExeFunc

regReg fn [r,des] _ proc = do r' <- readIORef (proc ! r)
                              let des' = fn r'
                              writeIORef (proc ! des) des'
                              return Continue

li, lw, sw, exit, b, br, syscall :: ExeFunc

li [imm,des] _ proc = do writeIORef (proc ! des) imm
                         return Continue

lw [r,off,des] ram proc = do addr <- readIORef (proc ! r)
                             let addr' = addr + off
                             load <- readIORef (ram ! addr')
                             writeIORef (proc ! des) load
                             return Continue

sw [r,des,off] ram proc = do addr <- readIORef (proc ! des)
                             let addr' = addr + off
                             store <- readIORef (proc ! r)
                             writeIORef (ram ! addr') store
                             return Continue

exit _ _ _ = return Exit

b [jump] _ proc = do modifyIORef (proc ! 15) (+jump)
                     return Continue

br [r] _ proc = do r' <- readIORef (proc ! r)
                   writeIORef (proc ! 15) r'
                   return Continue

bRegReg, bRegImm :: (Int64 -> Int64 -> Bool) -> ExeFunc

bRegReg fn [r0,r1,jump] _ proc = do r0' <- readIORef (proc ! r0)
                                    r1' <- readIORef (proc ! r1)
                                    case r0' `fn` r1' of
                                      True  -> modifyIORef (proc ! 15) (+jump)
                                      False -> return ()
                                    return Continue

bRegImm fn [r,imm,jump] _ proc = do r' <- readIORef (proc ! r)
                                    case r' `fn` imm of
                                      True  -> modifyIORef (proc ! 15) (+jump)
                                      False -> return ()
                                    return Continue

syscall [i] ram proc = do let fn = getSyscallFn i
                          fn ram proc

getSyscallFn :: Int64 -> RAM -> Proc -> IO Sig

getSyscallFn i = case i of
                   0  -> pChar
                   1  -> pString
                   2  -> pInt
                   3  -> pUnsigned
                   4  -> rChar
                   5  -> rString
                   6  -> rInt
                   7  -> rUnsigned

pChar, pString, pInt, pUnsigned, rChar, rString, rInt, rUnsigned :: RAM -> Proc -> IO Sig

pChar _ proc = do c <- readIORef (proc ! 0)
                  let c' = chr $ fromIntegral c
                  putChar c'
                  return Continue

pString ram proc = do addr <- readIORef (proc ! 0)
                      modifyIORef (proc ! 0) succ
                      c <- readIORef (ram ! addr)
                      let c' = chr $ fromIntegral c
                      case c' of
                        '\NUL' -> return Continue
                        _      -> do putChar c'
                                     pString ram proc

pInt _ proc = do i <- readIORef (proc ! 0)
                 putStr $ show i
                 return Continue

pUnsigned _ proc = do i <- readIORef (proc ! 0)
                      let u = fromIntegral i :: Word64
                      putStr $ show u
                      return Continue

rChar _ proc = do c <- hGetChar stdin
                  let i = fromIntegral $ fromEnum c :: Int64
                  writeIORef (proc ! 0) i
                  return Continue

rString ram proc = do s <- hGetLine stdin
                      writeString s
  where
  writeString :: String -> IO Sig
  writeString (x:xs) = do addr <- readIORef (proc ! 0)
                          modifyIORef (proc ! 0) succ
                          writeIORef (ram ! addr) (fromIntegral $ fromEnum x)
                          writeString xs
  writeString [] = do addr <- readIORef (proc ! 0)
                      writeIORef (ram ! addr) 0
                      return Continue

rInt ram proc = do s <- hGetLine stdin
                   let i = read s :: Int64
                   writeIORef (proc ! 0) i
                   return Continue

rUnsigned ram proc = do s <- hGetLine stdin
                        let u = read s :: Word64
                        writeIORef (proc ! 0) (fromIntegral u)
                        return Continue

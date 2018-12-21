module Util where

import Ubi

getReg :: Int32 -> Mode -> Set -> Regs -> IO Reg
getReg i Raw _ regs = return $ regs ! i
getReg i Easy set _ = readIORef $ set ! i

mask :: Int -> Int32
mask amount = 2 ^ amount - 1

pop :: Stack -> IO (Maybe Processor)
pop (Stack n arr) = do n' <- readIORef n
                       case n' of
                         0   -> return Nothing
                         n'' -> do let n''' = pred n''
                                   writeIORef n n'''
                                   readIORef (arr ! n''')

push :: Processor -> Stack -> IO ()
push proc (Stack n arr) = do n' <- readIORef n
                             writeIORef (arr ! n') proc
                             modifyIORef n succ

signed :: Int -> Int32 -> Int32
signed size value = let bit   = size - 1
                        mask' = mask bit
                    in case value `testBit` bit of
                         True  -> value .|. complement mask'
                         False -> value .&. mask'

unsigned :: Int -> Int32 -> Int32
unsigned size value = mask size .&. value

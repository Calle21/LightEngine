module Util where

import qualified Data.ByteString.Char8 as C
import Ubi

infixr 8 &&&
(&&&) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
f0 &&& f1 = \a -> f0 a && f1 a

elemIndex' :: C.ByteString -> Array Int C.ByteString -> Maybe Int
elemIndex' s arr = loop (bounds arr)
  where
  loop :: (Int,Int) -> Maybe Int
  loop (i,hi) | i > hi       = Nothing
              | arr ! i == s = Just i
              | otherwise    = loop (i + 1,hi)

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

safeTail' :: C.ByteString -> C.ByteString
safeTail' s | C.null s  = s
            | otherwise = C.tail s

signed :: Int -> Int32 -> Int32
signed size value = let bit   = size - 1
                        mask' = mask bit
                    in case value `testBit` bit of
                         True  -> value .|. complement mask'
                         False -> value .&. mask'

sumOn :: Num b => (a -> b) -> [a] -> b
sumOn _  []     = 0
sumOn fn (x:xs) = fn x + sumOn fn xs

unsigned :: Int -> Int32 -> Int32
unsigned size value = mask size .&. value

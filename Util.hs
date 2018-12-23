module Util where

import qualified Data.ByteString.Char8 as C
import Ubi

elemIndex' :: C.ByteString -> Array Int C.ByteString -> Maybe Int
elemIndex' s arr = loop (bounds arr)
  where
  loop :: (Int,Int) -> Maybe Int
  loop (i,hi) | i > hi       = Nothing
              | arr ! i == s = Just i
              | otherwise    = loop (i + 1,hi)

getReg :: Int32 -> Int32 -> Int32 -> Processor -> RAM -> IO Register
getReg reg 0 _      proc _   = return $ proc ! reg
getReg reg 1 offset proc ram = do addr <- (+offset) `fmap` readIORef (proc ! reg)
                                  return $ ram ! addr
                              

listDirectory' :: FilePath -> IO [FilePath]
listDirectory' path = map (path </>) `fmap` listDirectory path

listToArray :: [a] -> Array Int a
listToArray xs = listArray (0, length xs - 1) xs

map2 :: (a -> b -> c) -> [a] -> [b] -> [c]
map2 _  []     []     = []
map2 fn (x:xs) (y:ys) = fn x y : map2 fn xs ys

mask :: Int -> Int32
mask amount = 2 ^ amount - 1

pop :: Pool -> IO (Maybe Processor)
pop (Pool n arr) = do n' <- readIORef n
                       case n' of
                         0   -> return Nothing
                         n'' -> do let n''' = pred n''
                                   writeIORef n n'''
                                   readIORef (arr ! n''')

push :: Processor -> Pool -> IO ()
push proc (Pool n arr) = do n' <- readIORef n
                             writeIORef (arr ! n') proc
                             modifyIORef n succ

putFirst :: a -> [a] -> [a]
putFirst elt xs | elt `elem` xs = elt : delete elt xs
                | otherwise     = xs

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

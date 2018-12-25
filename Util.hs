module Util where

import qualified Data.ByteString.Char8 as C
import Types
import Ubi

elemIndex' :: C.ByteString -> Array Int C.ByteString -> Maybe Int
elemIndex' s arr = loop (bounds arr)
  where
  loop :: (Int,Int) -> Maybe Int
  loop (i,hi) | i > hi       = Nothing
              | arr ! i == s = Just i
              | otherwise    = loop (i + 1,hi)

listDirectory' :: FilePath -> IO [FilePath]
listDirectory' path = map (path </>) `fmap` listDirectory path

map2 :: (a -> b -> c) -> [a] -> [b] -> [c]
map2 _  []     []     = []
map2 fn (x:xs) (y:ys) = fn x y : map2 fn xs ys

putFirst :: a -> [a] -> [a]
putFirst elt xs | elt `elem` xs = elt : delete elt xs
                | otherwise     = xs

safeTail' :: C.ByteString -> C.ByteString
safeTail' s | C.null s  = s
            | otherwise = C.tail s

sumOn :: Num b => (a -> b) -> [a] -> b
sumOn _  []     = 0
sumOn fn (x:xs) = fn x + sumOn fn xs

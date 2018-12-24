module Util where

import qualified Data.ByteString.Char8 as C
import Types
import Ubi

decode :: DecodeType -> Int -> Int32 -> (Int32,Int32)
decode Unsigned amount value = (shiftR value (32 - amount), shiftL value amount)
decode Signed   amount value = let sign    = value .&. minBound
                                   cleared = clearBit value 31
                               in (sign .|. cleared `shiftR` (32 - amount), shiftL value amount)

elemIndex' :: C.ByteString -> Array Int C.ByteString -> Maybe Int
elemIndex' s arr = loop (bounds arr)
  where
  loop :: (Int,Int) -> Maybe Int
  loop (i,hi) | i > hi       = Nothing
              | arr ! i == s = Just i
              | otherwise    = loop (i + 1,hi)

getReg :: Int32 -> Int32 -> Int32 -> Regs -> RAM -> IO Register
getReg ix 0 _      regs _   = return $ regs ! ix
getReg ix 1 offset regs ram = do addr <- (+offset) `fmap` readIORef (regs ! ix)
                                 return $ ram ! addr
                              

listDirectory' :: FilePath -> IO [FilePath]
listDirectory' path = map (path </>) `fmap` listDirectory path

listToArray :: [a] -> Array Int32 a
listToArray xs = listArray (0, length xs - 1) xs

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

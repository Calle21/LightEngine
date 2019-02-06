module Util where

import qualified Data.ByteString.Char8 as C
import Types
import Ubi

 -- Decode

decode :: DecodeType -> Int -> Int64 -> (Int64,Int64)
decode Unsigned amount value = (shiftR value (64 - amount), shiftL value amount)
decode Signed   amount value = let sign    = value .&. minBound
                                   cleared = clearBit value 63
                               in (sign .|. cleared `shiftR` (64 - amount), shiftL value amount)

 -- dropEmptyLines

dropEmptyLines :: C.ByteString -> (C.ByteString, Int)
dropEmptyLines = rec (1 :: Int)
  where
  rec n s = let (t,s') = getNextTokenOnLine s
            in case t of
                 Nothing -> rec (dropped + 1) s'
                 Just _  -> (s,dropped)

 -- ElemIndex

elemIndex' :: C.ByteString -> Array Int C.ByteString -> Maybe Int
elemIndex' s arr = loop (bounds arr)
  where
  loop :: (Int,Int) -> Maybe Int
  loop (i,hi) | i > hi       = Nothing
              | arr ! i == s = Just i
              | otherwise    = loop (i + 1,hi)

getJump :: Int64 -> Int64
getJump v = fst $ decode Signed 16 v

getPlace :: Int64 -> Regs -> Ram -> IO (Reg, Int64)
getPlace args regs ram = do
  let (ix,         args')    = decode Unsigned 5 args
      (offsetMode, args'')   = decode Unsigned 1 args'
      (depth,      args''')  = decode Unsigned 1 args''
      (offset,     args'''') = decode Unsigned 5 args'''
  offset' <- case offsetMode of
               0 -> offset
               1 -> readIORef (regs ! offset)
  reg <- get (regs ! ix) depth offset'
  return (reg, args'''')
  where
  get :: Reg -> Int64 -> Int64 -> IO Register
  get reg 0     _      = return reg
  get reg 1     offset = do addr <- (+offset) `fmap` readIORef reg
                            return $ ram ! addr

listDirectory' :: FilePath -> IO [FilePath]
listDirectory' path = map (path </>) `fmap` listDirectory path

listToArray :: [a] -> Array Int64 a
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

 -- whiteSpace

whiteSpace :: Char -> Bool
whiteSpace c = c == ' ' || c == '\t'

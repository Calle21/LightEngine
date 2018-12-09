module ReadWrite where

import qualified Data.ByteString as B
import Types
import Ubi
import Util (bitSet)

readBinary :: FilePath -> IO RAM
readBinary path = bind =<< B.readFile path
  where
  bind :: B.ByteString -> IO RAM
  bind s = binTree `fmap` loop 0
    where
    loop :: Int -> IO [Register]
    loop n | n == length s = return []
           | otherwise     = do next <- loop (n + 3)
                                let a = s `B.index` n
                                    b = s `B.index` (n + 1) `shiftL` 8
                                    c = s `B.index` (n + 2) `shiftL` 16
                                    d = a .|. b .|. c
                                reg <- word32ToRegister d
                                return (reg : next)
      where
      word32ToRegister :: Word32 -> IO Register
      word32ToRegister w = bind =<< register
        where
        bind :: Register -> IO Register
        bind reg = loop 0 w >> return reg
          where
          loop :: Int -> Word32 -> IO ()
          loop 20 _ = return ()
          loop i  w = writeIORef (reg ! i) (odd w) >> loop (i + 1) (w `shiftR` 1)

writeBinary :: FilePath -> RAM -> IO ()
writeBinary path ram = do word32List <- mapM registerToWord32 (binTreeToList ram)
                          B.writeFile path $ B.pack (listIt word32List)
  where
  listIt :: [Word32] -> [Word8]
  listIt []     = []
  listIt (x:xs) = let a = x .&. mask 8
                      b = x `shiftR` 8 .&. mask 8
                      c = x `shiftR` 16
                  in a : b : c : listIt xs
  registerToWord32 :: Register -> IO Word32
  registerToWord32 reg = loop 0 0
    where
    loop :: Int -> Word32 -> IO Word32
    loop 20 w = return w
    loop i  w = (bitSet w i) `fmap` readIORef (reg ! i) >>= loop (i + 1)

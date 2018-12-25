module Assembler.Write where

import Ubi
import Util

writeFile' :: FilePath -> [Int32] -> IO ()
writeFile' path file = writeFile path $ concatMap chopIt file
  where
  chopIt :: Int32 -> [Char]
  chopIt w = [conv $ w `shiftR` 24, conv $ unsigned 8 (w `shiftR` 16), conv $ unsigned 8 (w `shiftR` 8), conv $ unsigned 8 w]
    where
    conv = toEnum . fromIntegral

readFile' :: FilePath -> IO [Int32]
readFile' path = do s <- readFile path
                    return $ unchop s
  where
  unchop :: [Char] -> [Int32]
  unchop (a:b:c:d:xs) = (conv a `shiftL` 24 .|. conv b `shiftL` 16 .|. conv c `shiftL` 8 .|. conv d) : unchop xs
  unchop []           = []
    where
    conv = fromIntegral . fromEnum

module Assembler.Write where

import Control.Exception (bracket)
import System.IO (AbsoluteSeek, hClose, hPutStr, hSeek, WriteMode)
import Ubi
import Util

writeLexed :: FilePath -> [[Int32]] -> IO ()
writeLexed path files = bracket
                          (openFile path WriteMode)
                          hClose
                          (\handle -> do hSeek handle AbsoluteSeek 4000
                                         mapM_ (hPutStr handle . concatMap chopIt) files)
  where
  chopIt :: Int32 -> [Char]
  chopIt w = [conv $ w `shiftR` 24, conv $ unsigned 8 (w `shiftR` 16), conv $ unsigned 8 (w `shiftR` 8), conv $ unsigned 8 w]
    where
    conv = toEnum . fromIntegral

readWritten :: FilePath -> IO [Int32]
readWritten path = do s <- readFile path
                      return $ unchop s
  where
  unchop :: [Char] -> [Int32]
  unchop (a:b:c:d:xs) = (conv a `shiftL` 24 .|. conv b `shiftL` 16 .|. conv c `shiftL` 8 .|. conv d) : unchop xs
  unchop []           = []
    where
    conv = fromIntegral . fromEnum

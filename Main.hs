module Main where

import qualified LightEngine.24Bit as Small
import qualified LightEngine.32Bit as Mid
import qualified LightEngine.40Bit as Big
import Ubi

main :: IO ()
main = do args <- getArgs
          case args of
            ["24", path] -> case doesDirectoryExist path of
                              True  -> Small.assemble path >>= Small.execute
                              False -> Small.readFile' path >>= Small.execute
            ["32", path] -> case doesDirectoryExist path of
                              True  -> Mid.assemble path >>= Mid.execute
                              False -> Mid.readFile' path >>= Mid.execute
            ["40", path] -> case doesDirectoryExist path of
                              True  -> Big.assemble path >>= Big.execute
                              False -> Big.readFile' path >>= Big.execute
            _            -> C.putStrLn (C.pack "Try 'LightEngine xx *path*' where (xx) marks architecture (24, 32 or 40) and *path* points to an executable file or directory containing source files")

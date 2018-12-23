module Main where

import Assembler.Assemble
import Execute
import Ubi

main :: IO ()
main = do args <- getArgs
          case args of
            [path] -> case doesDirectoryExist path of
                        True  -> assemble path >>= execute
                        False -> readFile' path >>= execute
            _      -> C.putStrLn (C.pack "Pass path to executable or directory containing source files")

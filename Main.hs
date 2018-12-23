module Main where

import Assembler.Assemble
import Execute
import Ubi

main :: IO ()
main = do args <- getArgs
          case args of
            [path] -> assemble path >>= execute
            _      -> C.putStrLn (C.pack "Pass path to directory containing source files")

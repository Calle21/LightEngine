module Main where

import Assembler.Assemble (assemble)
import Assembler.Write (readWritten)
import Execute
import System.Environment (getArgs)

main :: IO ()
main = do args <- getArgs
          case args of
            ["help"]    -> C.putStrLn (C.pack "Try 'LightEngine as *path*' and 'LightEngine *exe*' to run something")
            ["as",path] -> assemble path
            [exe]       -> execute =<< readWritten exe
            _           -> return ()

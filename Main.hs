module Main where

import Assembler.Check (check)
import Assembler.Clean (clean)
import Assembler.Compact (compact)
import Assembler.Concat (concat')
import Assembler.GetLabs (getLabs)
import Assembler.Lex (lex')
import Assembler.Prepare (prepare)
import Assembler.Replace (replace)
import Assembler.Split (split)
import Assembler.Tidy (tidy)
import Execute.Execute (execute)
import Types
import Ubi
import Util

main :: IO ()
main = do [path, test] <- getArgs
          dir          <- doesDirectoryExist path
          sourcePaths  <- case dir of
                           True  -> do contents <- listDir path
                                       return $ filter (\p -> takeExtension p == ".s") contents
                           False -> return [path]
          strings     <- mapM readFile sourcePaths
          let lexed     = map lex' (zip (map takeFileName sourcePaths) strings)
              checked   = map check lexed
              split'    = map split checked
              tidy'     = map tidy split'
              replaced  = map replace tidy'
              concat''  = concat' replaced
              gotLabs   = getLabs concat''
              cleaned   = clean gotLabs
              compacted = compact cleaned
          ram <- prepare compacted
          case test of
            "t" -> print cleaned
            "r" -> execute ram

listDir :: FilePath -> IO [FilePath]
listDir path = map (path </>) `fmap` listDirectory path

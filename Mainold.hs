module Main where

import Assembler.Check
import Assembler.Clean
import Assembler.Compact
import Assembler.Concat
import Assembler.GetLabs
import Assembler.Lex
import Assembler.Prepare
import Assembler.RemovePunct
import Assembler.Replace
import Assembler.Split
import Assembler.Tidy
import Execute.Execute
import Share
import Types
import Ubi

main :: IO ()
main = do [path, test] <- getArgs
          dir          <- doesDirectoryExist path
          sourcePaths  <- case dir of
                            True  -> do contents <- listDir path
                                        return $ filter (\p -> takeExtension p == ".s") contents
                            False -> return [path]
          strings     <- mapM readFile sourcePaths
          let lexed        = map lex' (zip (map takeFileName sourcePaths) strings)
              checked      = map check lexed
              split'       = map split checked
              withoutPunct = map removePunct split'
              tidy'        = map tidy withoutPunct
              replaced     = map replace tidy'
              concat''     = concat' replaced
              gotLabs      = getLabs concat''
              cleaned      = clean gotLabs
              compacted    = compact cleaned
          (ram,proc) <- prepare compacted
          case test of
            "t" -> print cleaned
            "r" -> execute ram proc
            "l" -> print lexed

listDir :: FilePath -> IO [FilePath]
listDir path = map (path </>) `fmap` listDirectory path

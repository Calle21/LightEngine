module Main where

import System.Directory (listDirectory, doesDirectoryExist)
import System.Environment (getArgs)
import System.FilePath.Posix ((</>), takeExtension)

import Compile.MakeLines (makeLines)


main :: IO ()
main = do [target] <- fmap (\args -> if null args then ["."] else args) getArgs
          files <- listRec target
          mapM print files
          return ()

listRec :: FilePath -> IO [(FilePath,String)]
listRec path = do dir <- doesDirectoryExist path
                  case dir of
                    True  -> do contents <- fmap (map (path </>)) (listDirectory path)
                                fmap concat $ mapM listRec contents
                    False -> case takeExtension path of
                               ".s" -> do contents <- readFile path
                                          return [(path,contents)]
                               _    -> return []

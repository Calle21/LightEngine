module Assembler.Assemble where

import Assembler.Read
import qualified Data.ByteString.Char8 as C
import Ubi
import Util

assemble :: FilePath -> IO RAM
assemble path = do contents <- listDirectory' path
                   let sourcePaths = sort $ filter (\p -> takeExtension p == ".s") contents
                   if head sourcePaths == path </> "Main.s"
                   then do bstrings <- mapM C.readFile sourcePaths
                           let sizes       = map getFileSize bstrings
                               symbolTable = zip (map takeBaseName sourcePaths)
                                                 (scanl' (+) 0 $ init sizes)
                               lexed       = map2 (read' symbolTable) (map takeFileName sourcePaths)
                                                                       bstrings
                           return $ listToArray $ concat lexed
                   else
                     error "Could not find 'Main.s'"
  where
  getFileSize :: Integral a => C.ByteString -> a
  getFileSize s | C.null s  = 0
                | otherwise = let (filled,s') = isLineFilled s
                              in fromIntegral (fromEnum filled) + getFileSize s'
    where
    isLineFilled :: C.ByteString -> (Bool,C.ByteString)
    isLineFilled s = let s'  = C.dropWhile (==' ') s
                         s'' = safeTail' $ C.dropWhile (/='\n') s'
                     in (C.head s' `elem` "\n-", s'')

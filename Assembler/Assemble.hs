module Assembler.Assemble where

import Assembler.Read
import Assembler.Write (writeLexed)
import qualified Data.ByteString.Char8 as C
import System.Directory
import System.FilePath.Posix
import Ubi
import Util (listDirectory', map2, safeTail')

assemble :: FilePath -> IO ()
assemble path = do contents <- listDirectory' path
                   let mpath       = path </> "main.s"
                       sourcePaths = putFirst mpath $ filter (\p -> takeExtension p == ".s") contents
                   if head sourcePaths == mpath
                   then do let objPaths = filter (\p -> takeExtension p == ".o") contents
                           bstrings <- mapM C.readFile sourcePaths
                           let sizes       = map (getFileSize 0) bstrings
                               symbolTable = zip (map takeBaseName sourcePaths)
                                                 scanl' (+) 1000 (init sizes)
                               lexed       = map2 (read' symbolTable) (map takeFileName sourcePaths)
                                                                       bstrings
                           writeLexed (takeFileName path ++ ".exe") lexed
                   else
                     error "Could not find 'main.s'"
  where
  getFileSize :: Int32 -> C.ByteString -> Int32
  getFileSize size s | C.null s  = size
                     | otherwise = let (filled,s') = isLineFilled s
                                   in getFileSize (fromIntegral (fromEnum filled) + size) s'
    where
    isLineFilled :: C.ByteString -> (Bool,C.ByteString)
    isLineFilled s = let s'  = C.dropWhile (==' ') s
                         s'' = safeTail' $ C.dropWhile (/='\n') s'
                     in (C.head s' `elem` "\n-", s'')

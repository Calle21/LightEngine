module Assembler.Error where

import qualified Data.ByteString.Char8 as C
import System.IO (hPrint, hPutStr, stderr)

readError :: String -> Int -> C.ByteString -> IO ()
readError filename ln message = do C.hPutStr stderr (C.pack "Read error in file ")
                                   hPutStr stderr filename
                                   C.hPutStr stderr (C.pack " on line ")
                                   hPrint stderr ln
                                   C.hPutStr stderr (C.pack ":\n   ")
                                   C.hPutStrLn stderr message
                                   error "Lexing failed"

module Assembler.Error where

import qualified Data.ByteString.Char8 as C
import System.IO (hPrint, stderr)

readError :: Int -> C.ByteString -> IO ()
readError ln message = do C.hPutStr stderr (C.pack "Read error on line ")
                          hPrint stderr ln
                          C.hPutStr stderr (C.pack "\n   ")
                          C.hPutStrLn stderr message
                          error "Lexing failed"

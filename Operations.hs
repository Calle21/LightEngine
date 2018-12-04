module Operations where

import qualified Data.ByteString.Char8 as C
import Types
import Ubi

operations :: Array Int (C.ByteString, Int64 -> IO ())
operations = M.fromList [(C.Pack "add", std2op (+))
                       , (C.Pack "sub", std2op (-))
                       , (C.Pack "mul", std2op (*))
                       , (C.Pack "div", std2op div)
                       , (C.Pack "mod", std2op mod)
                       , (C.Pack "and", std2op (.&.))
                       , (C.Pack "or",  std2op (.|.))
                       , (C.Pack "xor", std2op xor)
                       ]
  where
  std2op :: (Int64 -> Int64 -> Int64) -> Int64 -> IO ()
  std2op op arg = let (r0, r1, des) = get3 arg
                  in do r0' <- readReg r0
                        r1' <- readReg r1
                        let des' = r0' `op` r1'
                        writeReg des des'

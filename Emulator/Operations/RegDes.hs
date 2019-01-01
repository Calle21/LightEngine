module Emulator.Operations.RegRegDes where

import Types
import Ubi
import Util (decode, getReg)

regDes :: (Int32 -> Int32 -> Int32) -> Operation
regDes op proc ram args = do
  let (ix0,     args')     = decode Unsigned 4 args
      (mode0,   args'')    = decode Unsigned 1 args'
      (offset0, args''')   = decode Unsigned 8 args''
      (ix1,     args'''')  = decode Unsigned 4 args'''
      (mode1,   args''''') = decode Unsigned 1 args''''
      (offset1, _)         = decode Unsigned 9 args'''''
  reg <- getReg ix0 mode0 offset0 proc ram
  des <- getReg ix1 mode1 offset1 proc ram
  writeIORef des `fmap` liftM op (readIORef reg)
  return Continue

move, not' :: Operation

move = regDes id

not' = regDes complement

module Emulator.Operations.Firm.32Bit where

import Types.32Bit
import Util.32Bit (decode, listToArray)

firm :: Operation
firm proc ram args = do
  let (id, args') = decode Unsigned 7 args
  firms !! id $ proc ram args'

firms : listToArray $ [putChar]

putChar :: Operation

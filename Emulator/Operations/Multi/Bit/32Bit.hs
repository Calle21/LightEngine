module Emulator.Operations.Multi.32Bit where

import Types.32Bit

multi :: Operation
multi proc ram args = do
  let (which,args') = decode Unsigned 3 args
  multiOps ! which $ proc ram args'

multiOps = listToArray [clearBit, convert, fact, move, not, setBit, short, toggleBit]

 -- Templates

bitOp :: (Int32 -> Int -> Int32) -> Operation
bitOp op (Proc regs _) ram args = do
  let (ix0,     args')       = decode Unsigned 4 args
      (mode0,   args'')      = decode Unsigned 1 args'
      (offset0, args''')     = decode Unsigned 4 args''
      (bit,     args'''')    = decode Unsigned 5 args'''
      (ix1,     args''''')   = decode Unsigned 4 args''''
      (mode1,   args'''''')  = decode Unsigned 1 args'''''
      (offset1, args''''''') = decode Unsigned 5 args''''''
  reg <- getReg ix0 mode0 offset0 regs ram
  des <- getReg ix1 mode1 offset1 regs ram
  value <- readIORef reg
  writeIORef des $ op value (fromIntegral bit)
  return Continue

regDes :: (Int32 -> Int32 -> Int32) -> Operation
regDes op proc ram args = do
  let (ix0,     args')     = decode Unsigned 4 args
      (mode0,   args'')    = decode Unsigned 1 args'
      (offset0, args''')   = decode Unsigned 7 args''
      (ix1,     args'''')  = decode Unsigned 4 args'''
      (mode1,   args''''') = decode Unsigned 1 args''''
      (offset1, _)         = decode Unsigned 7 args'''''
  reg <- getReg ix0 mode0 offset0 proc ram
  des <- getReg ix1 mode1 offset1 proc ram
  writeIORef des `fmap` liftM op (readIORef reg)
  return Continue

 -- Ops

clearBit, setBit, toggleBit, convert, fact, move, not', short :: Operation

clearBit' = bitOp clearBit

setBit' = bitOp setBit

toggleBit = \i b -> bit b `xor` i

convert (Proc regs _) ram args = do
  let (way,     args')      = decode Unsigned 1 args
      (ix0,     args'')     = decode Unsigned 4 args'
      (mode0,   args''')    = decode Unsigned 1 args''
      (offset0, args'''')   = decode Unsigned 6 args'''
      (ix1,     args''''')  = decode Unsigned 4 args''''
      (mode1,   args'''''') = decode Unsigned 1 args'''''
      (offset1, _)          = decode Unsigned 7 args''''''
  reg <- getReg ix0 mode0 offset0 regs ram
  des <- getReg ix1 mode1 offset1 regs ram
  value <- readIORef reg
  writeIORef des $ [\i -> unsafeCoerce (fromIntegral i :: Float)
                  , \f -> floor (unsafeCoerce f :: Float)] !! way $ value
  return Continue

fact = regDes factorial
  where
  factorial v | v < 0     = error "Factorial of negative number"
              | otherwise = loop v
    where
    loop 0 = 1
    loop v = v * (v - 1)

move = regDes id

not' = regDes not

short proc ram args = do
  let (which,args') = decode Unsigned 1 args
  [return',wait] !! which $ proc ram args'
  return Continue
  where
  return', wait :: Operation
  return' (Proc regs _) _ args = do
    let (immp, args') = decode Unsigned 1 args
    if immp == 1
    then do let (imm,_) = decode Signed 22 args'
            writeIORef (regs ! 0) imm
    else return ()
    return Return
  wait (Proc regs callIO) _ args = do let (ix,_) = decode Unsigned 4 args
                                      calls <- readIORef callIO
                                      case M.lookup ix calls of
                                        Nothing   -> error "No call waiting"
                                        Just call -> do writeIORef callIO $ M.remove ix calls
                                                        result <- call
                                                        writeIORef (proc ! ix) result
                                                        return Continue

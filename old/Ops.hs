module Ops (ops) where

import qualified Register.Op as Op
import Types

ops :: BinTree Operation
ops = unsafePerformIO $ binTree [add,and,or]

 -- ops

add,and,or :: Processor -> RAM -> Register -> IO Signal

add proc ram args = do off <- fetch offRegFetch proc
                       stack <- fetch stackRegFetch proc
                       addr <- fetch addrRegFetch proc
                       args' <- Op.mov args off
                       Op.add stack off addr
                       in0 <- fetch addr ram
                       args'' <- Op.mov args' off
                       Op.add stack off addr
                       in1 <- fetch addr ram
                       Op.mov args'' off
                       Op.add stack off addr
                       out <- fetch addr ram
                       Op.add in0 in1 out

and proc ram args = do off <- fetch offRegFetch
                       stack <- fetch stackRegFetch proc
                       addr <- fetch addrRegFetch proc
                       args' <- Op.mov args off
                       Op.add stack off addr
                       in0 <- fetch addr ram
                       args'' <- Op.mov args' off
                       Op.add stack off addr
                       in1 <- fetch addr ram
                       Op.mov args'' off
                       Op.add stack off addr
                       out <- fetch addr ram
                       Op.and in0 in1 out

or proc ram args = do off <- fetch offRegFetch
                      stack <- fetch stackRegFetch proc
                      addr <- fetch addrRegFetch proc
                      args' <- Op.mov args off
                      Op.add stack off addr
                      in0 <- fetch addr ram
                      args'' <- Op.mov args' off
                      Op.add stack off addr
                      in1 <- fetch addr ram
                      Op.mov args'' off
                      Op.add stack off addr
                      out <- fetch addr ram
                      Op.or in0 in1 out

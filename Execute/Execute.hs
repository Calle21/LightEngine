module Execute.Execute where

import Assembler.OpInfo (fetchOperation, getPackSyntax)
import Types
import Ubi
import Util

execute :: (RAM, Proc) -> IO ()
execute (ram, proc) = do ic <- readIORef (proc ! 15)
                         writeIORef (proc ! 15) (succ ic)
                         fetch <- readIORef (ram ! ic)
                         writeIORef (proc ! 14) fetch
                         sig <- executeOperation fetch ram proc
                         case sig of
                           Continue -> execute (ram, proc)
                           Exit     -> return ()

executeOperation :: Int64 -> RAM -> Proc -> IO Sig
executeOperation op ram proc =
  let (op':args) = unpack op
  in  fetchOperation op' args ram proc

unpack :: Int64 -> [Int64]
unpack op = let (opi,op') = decode 5 op
                syn       = getPackSyntax opi
            in opi : recArgs op' syn
  where
  recArgs :: Int64 -> [Int64] -> [Int64]
  recArgs args (x:xs) = let (arg,args') = decode x args
                        in arg : recArgs args' xs
  recArgs _    []     = []

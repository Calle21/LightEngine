module Register.Op where

import qualified Transistor as T
import Types
import Ubi
import Util

add :: Register -> Register -> Register -> IO ()
add r0 r1 des = loop False (reverse r0) (reverse r1) (reverse des)
  where
  loop :: Bool -> Register -> Register -> Register -> IO ()
  loop _     []     []      _     = return ()
  loop carry xs     []     zs     = (if carry then inc xs else return ()) >> mov xs zs
  loop carry []     ys     zs     = (if carry then inc ys else return ()) >> mov ys zs
  loop carry (x:xs) (y:ys) (z:zs) = do a <- readIORef x
                                       b <- readIORef y
                                       let (r,carry') = count a b carry ! listArray (0,3) [(False,False),(True,False),(False,True),(True,True)]
                                       writeIORef z r
                                       loop carry' xs ys zs
    where
    count a b c = fromEnum a + fromEnum b + fromEnum c

and :: Register -> Register -> Register -> IO ()
and []     _      _      = return ()
and (x:xs) (y:ys) (z:zs) = T.and x y z >> and xs ys zs

cmp :: Register -> Register -> IO Ordering
cmp []     _      = return EQ
cmp (x:xs) (y:ys) = do a <- readIORef x
                       b <- readIORef y
                       case compare a b of
                         LT  -> return LT
                         EQ  -> cmp xs ys
                         GT  -> return GT

dec :: Register -> IO ()
dec r = loop (reverse r)
  where
  loop :: Register -> IO ()
  loop []     = return ()
  loop (x:xs) = do b <- readIORef x
                   if b then writeIORef x False
                        else writeIORef x True >> dec xs

eq :: Register -> Register -> IO Bool
eq []     _      = return True
eq (x:xs) (y:ys) = do b0 <- readIORef x
                      b1 <- readIORef y
                      if b0 == b1 then eq xs ys
                                  else return False

even :: Register -> IO Bool
even r = not `fmap` readIORef (last r)

ez :: Register -> IO Bool
ez []     = return True
ez (x:xs) = do b <- readIORef x
               if b then return False
                    else loop xs

ge :: Register -> Register -> IO Bool
ge []     _      = return True
ge (x:xs) (y:ys) = do b0 <- readIORef x
                      b1 <- readIORef y
                      case compare b0 b1 of
                        GT  -> return True
                        EQ  -> ge xs ys
                        LT  -> return False

gt :: Register -> Register -> IO Bool
gt []     _      = return False
gt (x:xs) (y:ys) = do b0 <- readIORef x
                      b1 <- readIORef y
                      case compare b0 b1 of
                        GT  -> return True
                        EQ  -> gt xs ys
                        LT  -> return False

inc :: Register -> IO ()
inc r = loop (reverse r)
  where
  loop []     = return ()
  loop (x:xs) = do b <- readIORef x
                   if b then writeIORef x False >> loop xs
                        else writeIORef x True

mov :: Register -> Register -> IO Register
mov []     []     = return []
mov xs     []     = return xs
mov []     (y:ys) = writeIORef y False >> mov [] ys
mov (x:xs) (y:ys) = readIORef x >>= writeIORef y >> mov xs ys

not :: Register -> Register -> IO ()
not []     _      = return ()
not (x:xs) (y:ys) = T.not x y >> not xs ys

odd :: Register -> IO Bool
odd r = readIORef (last r)

or :: Register -> Register -> Register -> IO ()
or []     _      _      = return ()
or (x:xs) (y:ys) (z:zs) = T.or x y z >> or xs ys zs

sub :: Register -> Register -> Register -> IO ()
sub r0 r1 des = loop False (reverse r0) (reverse r1) des
  where
  loop :: Bool -> Register -> Register -> Register -> IO ()
  loop _     []     _      _      = return ()
  loop carry (x:xs) (y:ys) (z:zs) = do a <- readIORef x
                                       b <- readIORef y
                                       let (r,carry') = count a b carry ! listArray (-2,1) [(False,True),(True,True),(False,False),(True,False)]
                                       writeIORef z r
                                       loop carry' xs ys zs
    where
    count a b c = fromEnum a - fromEnum b - fromEnum c

xor :: Register -> Register -> Register -> IO ()
xor []     _      _      = return ()
xor (x:xs) (y:ys) (z:zs) = T.xor x y z >> xor xs ys zs

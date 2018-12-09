module Register.Op where

import Types
import Ubi
import Util

add :: Register -> Register -> Register -> IO ()
add r0 r1 des = loop False 0
  where
  loop :: Bool -> Int -> IO ()
  loop _ 20 = return ()
  loop c i  = do a <- readIORef (r0 ! i)
                 b <- readIORef (r1 ! i)
                 let (r,c') = count a b c ! listArray (0,3) [(False,False),(True,False),(False,True),(True,True)]
                 writeIORef (des ! i) r
                 loop c' (i + 1)
    where
    count a b c = boolToInt a + boolToInt b + boolToInt c

and :: Register -> Register -> Register -> IO ()
and r0 r1 des = loop 0
  where
  loop 20 = return ()
  loop i  = do a <- readIORef (r0 ! i)
               b <- readIORef (r1 ! i)
               writeIORef (des ! i) (a && b)
               loop (i + 1)

dec :: Register -> IO ()
dec r = loop 0
  where
  loop :: Int -> IO ()
  loop 20 = return ()
  loop i  = do b <- readIORef (r ! i)
               if b then writeIORef (r ! i) False
                    else writeIORef (r ! i) True >> loop (i + 1)

eq :: Register -> Register -> IO Bool
eq r0 r1 = loop 0
  where
  loop :: Int -> IO Bool
  loop 20 = return True
  loop i  = do b0 <- readIORef (r0 ! i)
               b1 <- readIORef (r1 ! i)
               if b0 == b1 then loop (i + 1)
                           else return False

even :: Register -> IO Bool
even r = not `fmap` readIORef (r ! 0)

ez :: Register -> IO Bool
ez r = loop 0
  where
  loop :: Int -> IO Bool
  loop 20 = return True
  loop i  = do b <- readIORef (r ! i)
               if b then return False
                    else loop (i + 1)

ge :: Register -> Register -> IO Bool
ge r0 r1 = loop 0
  where
  loop :: Int -> IO Bool
  loop 20 = return True
  loop i  = do b0 <- readIORef (r0 ! i)
               b1 <- readIORef (r1 ! i)
               case compare b0 b1 of
                 GT  -> return True
                 EQ  -> loop (i + 1)
                 LT  -> return False

gt :: Register -> Register -> IO Bool
gt r0 r1 = loop 0
  where
  loop :: Int -> IO Bool
  loop 20 = return False
  loop i  = do b0 <- readIORef (r0 ! i)
               b1 <- readIORef (r1 ! i)
               case compare b0 b1 of
                 GT  -> return True
                 EQ  -> loop (i + 1)
                 LT  -> return False

inc :: Register -> IO ()
inc r = loop 0
  where
  loop :: Int -> IO ()
  loop 20 = return ()
  loop i  = do b <- readIORef (r ! i)
               if b then writeIORef (r ! i) False >> loop (i + 1)
                    else writeIORef (r ! i) True


mov :: Register -> Register -> IO ()
mov r des = loop 0
  where
  loop 20 = return ()
  loop i  = do a <- readIORef (r0 ! i)
               writeIORef (des ! i) a
               loop (i + 1)

not :: Register -> Register -> IO ()
not r des = loop 0
  where
  loop 20 = return ()
  loop i  = do a <- readIORef (r0 ! i)
               writeIORef (des ! i) (not a)
               loop (i + 1)

odd :: Register -> IO Bool
odd r = readIORef (r ! 0)

or :: Register -> Register -> Register -> IO ()
or r0 r1 des = loop 0
  where
  loop 20 = return ()
  loop i  = do a <- readIORef (r0 ! i)
               b <- readIORef (r1 ! i)
               writeIORef (des ! i) (a || b)
               loop (i + 1)

sub :: Register -> Register -> Register -> IO ()
sub r0 r1 des = loop False 0
  where
  loop :: Bool -> Int -> IO ()
  loop _ 20 = return ()
  loop c i  = do a <- readIORef (r0 ! i)
                 b <- readIORef (r1 ! i)
                 let (r,c') = count a b c ! listArray (-2,1) [(False,True),(True,True),(False,False),(True,False)]
                 writeIORef (des ! i) r
                 loop c' (i + 1)
    where
    count a b c = boolToInt a - boolToInt b - boolToInt c

xor :: Register -> Register -> Register -> IO ()
xor r0 r1 des = loop 0
  where
  loop 20 = return ()
  loop i  = do a <- readIORef (r0 ! i)
               b <- readIORef (r1 ! i)
               writeIORef (des ! i) (count a b == 1)
               loop (i + 1)
    where
    count a b = boolToInt a + boolToInt b

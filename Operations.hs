module Operations where

import Types
import Ubi
import Util

addOp :: Register -> Register -> Register -> IO ()
addOp r0 r1 des = loop False 0
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

andOp :: Register -> Register -> Register -> IO ()
andOp r0 r1 des = loop 0
  where
  loop 20 = return ()
  loop i  = do a <- readIORef (r0 ! i)
               b <- readIORef (r1 ! i)
               writeIORef (des ! i) (a && b)
               loop (i + 1)

movOp :: Register -> Register -> IO ()
movOp r des = loop 0
  where
  loop 20 = return ()
  loop i  = do a <- readIORef (r0 ! i)
               writeIORef (des ! i) a
               loop (i + 1)

notOp :: Register -> Register -> IO ()
notOp r des = loop 0
  where
  loop 20 = return ()
  loop i  = do a <- readIORef (r0 ! i)
               writeIORef (des ! i) (not a)
               loop (i + 1)

orOp :: Register -> Register -> Register -> IO ()
orOp r0 r1 des = loop 0
  where
  loop 20 = return ()
  loop i  = do a <- readIORef (r0 ! i)
               b <- readIORef (r1 ! i)
               writeIORef (des ! i) (a || b)
               loop (i + 1)

subOp :: Register -> Register -> Register -> IO ()
subOp r0 r1 des = loop False 0
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

xorOp :: Register -> Register -> Register -> IO ()
xorOp r0 r1 des = loop 0
  where
  loop 20 = return ()
  loop i  = do a <- readIORef (r0 ! i)
               b <- readIORef (r1 ! i)
               writeIORef (des ! i) (count a b == 1)
               loop (i + 1)
    where
    count a b = boolToInt a + boolToInt b

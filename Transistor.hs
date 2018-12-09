module Transistor where

transistor :: IO Transistor
transistor = newIORef False

and, or, xor :: Transistor -> Transistor -> Transistor -> IO ()
and t0 t1 d = do b0 <- readIORef t0
                 b1 <- readIORef t1
                 writeIORef d (b0 && b1)

mov :: Transistor -> Transistor -> IO ()
mov t d = readIORef t >>= writeIORef d

not :: Transistor -> Transistor -> IO ()
not t d = not `fmap` readIORef t >>= writeIORef d

or :: Transistor -> Transistor -> Transistor -> IO ()
or t0 t1 d = do b0 <- readIORef t0
                b1 <- readIORef t1
                writeIORef d (b0 || b1)

xor :: Transistor -> Transistor -> Transistor -> IO ()
xor t0 t1 d = do b0 <- readIORef t0
                 b1 <- readIORef t1
                 writeIORef d (if b0 then not else id) b1

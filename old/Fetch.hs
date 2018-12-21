module Fetch where

import Types

fetch :: Register -> BinTree a -> IO (a, Register)
fetch []     (Node _ _) = error "Address ran out"
fetch xs     (Leaf t)   = return (t, xs)
fetch (x:xs) (Node l r) = do b <- readIORef x
                             if b then fetch xs r
                                  else fetch xs l

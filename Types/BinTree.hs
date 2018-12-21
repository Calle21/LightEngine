module Types.BinTree where

import Data.Bits(shiftR)

data BinTree a = Node {left  :: BinTree a
                     , right :: BinTree a}
               | Leaf a

binTree :: [a] -> IO (BinTree a)
binTree []  = error "Empty list"
binTree [x] = Leaf x
binTree xs  = do let len = length xs
                 add <- if even len then return 0
                                    else fromEnum `fmap` randomIO
                 let (left,right) = splitAt (len `shiftR` 1 + add) xs
                 l <- binTree left
                 r <- binTree right
                 return $ Node l r

binTreeToList :: BinTree a -> [a]
binTreeToList (Leaf x)   = [x]
binTreeToList (Node l r) = binTreeToList l ++ binTreeToList r

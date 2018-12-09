module Types.BinTree where

data BinTree a = Node {left  :: BinTree a
                     , right :: BinTree a}
               | Leaf a

binTree :: [a] -> IO (BinTree a)
binTree []  = error "Empty list"
binTree [x] = Leaf x
binTree xs  = do rand <- randomIO :: Bool
                 let len          = length xs
                     add          = boolToInt (odd len && rand)
                     mid          = (len `div` 2) + add
                     (left,right) = splitAt mid xs
                 left <- binTree left
                 right <- binTree right
                 return (Node left right)

binTreeToList :: BinTree a -> [a]
binTreeToList (Leaf x)   = [x]
binTreeToList (Node l r) = binTreeToList l ++ binTreeToList r

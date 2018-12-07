module Memory where

import Ubi
import Util

areas = [(1,4),(4,8),(8,12),(2,16),(1,20)]

memory = liftM listToArray $ makeAreas areas
  where
  makeAreas :: [(Int,Int)] -> IO [RAM]
  makeAreas []         = return []
  makeAreas ((n,s):xs) = do this <- replicateM n (makeMem s)
                            rest <- makeAreas xs
                            return (this ++ rest)
    where
    makeMem :: Int -> IO RAM
    makeMem s = liftM listToArray $ replicateM s register

module Assembler.Concat where

import Share
import Types
import Ubi

concat' :: [([[Token]],[[Token]])] -> ([[Token]],[[Token]])
concat' files = (concat $ map fst files, concat $ map snd files)

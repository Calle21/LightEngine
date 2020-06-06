module Assembler.Concat where

import Types
import Ubi
import Util

concat' :: [([[Token]],[[Token]])] -> ([[Token]],[[Token]])
concat' files = (concat $ map fst files, concat $ map snd files)

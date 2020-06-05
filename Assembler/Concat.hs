module Assembler.Concat where

import Types
import Ubi
import Util

concat' :: [Replaced] -> Concat
concat' files = (concat $ map fst files, concat $ map snd files)

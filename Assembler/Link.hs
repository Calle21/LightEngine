module Assembler.Link where

link :: [(String,Proc5)] -> Executable
link xs = let (filenames, instr) = unzip xs
              sizes              = map length instr
              offsets            = init $ scanl (+) 0 sizes
          in 


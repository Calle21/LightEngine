module Assembler.Order where

type SymTable = [(String,Int)]

type Ordered = ([Token], SymTable, SymTable, Checked)

order :: (String, Checked) -> (String, Ordered)
order (filename, file) = let labs            = getLabs 0 file
                             (llabs',glabs') = splitLabs [] [] labs
                             (dat, rest)   = getData 0 file
                             clean         = cleanUp rest
                         in (filename, (dat,llabs,glabs,clean))
  where
  getData :: Checked -> ([Token], Checked)
  getData ([a,b]:xs) = 
--  getRegs :: Int -> [Token] -> SymTable
--  getRegs _ []          = [("link",13),("sp",14),("ic",15)]
--  getRegs i (Name s:xs) = (s,i) : getRegs (i + 1) case xs of
--                                                    []      -> []
--                                                    (_:xs') -> xs'
  getLabs :: Int -> Checked -> [(Token,Int)]
  getLabs size (x:xs) = 
  splitLabs :: SymTable -> SymTable -> [(Token,Int)] -> (SymTable,SymTable)
  splitLabs acc0 acc1 ((t,i):xs) = case t of
                                     Llab s -> splitLabs ((s,i) : acc0) acc1 xs
                                     Glab s -> splitLabs acc0 ((s,i) : acc1) xs
            
  cleanUp :: Checked -> Checked
  cleanUp ([Llab _] : xs) = cleanUp xs
  cleanUp ([Glab _] : xs) = cleanUp xs
  cleanUp (x:xs)          = cleanInstr x : cleanUp xs
  cleanUp []              = []
    where
    cleanInstr :: [Token] -> [Token]
    cleanInstr (Name s:xs) = getOpi s : rec xs
      where
      rec (x:xs) | x == Comma || x == Arrow = rec xs
                 | otherwise                = case x of
                                                RegName s -> Reg (getRegi s) : rec xs
                                                _         -> x : rec xs

getRegi :: String -> Int64
getRegi s = case lookup s [("link",13),("sp",14),("ic",15)] of
              Just n  -> n
              Nothing -> error "Couldn't find register name"

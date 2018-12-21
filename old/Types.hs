module Types (
           module Types
         , module Types.BinTree
            ) where

import Data.BinTree

data Executable = Executable Int RAM

type Machine = BinTree MemoryArea

type MemoryArea = (RAM, BinTree RAM)

type Operation = Processor -> RAM -> Register -> IO Signal

type RAM = BinTree Register

type Register = [Transistor]

type Signal = Continue | Exit

type Transistor = IORef Bool

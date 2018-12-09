module Types (
           module Types
         , module Types.BinTree
            ) where

import Data.BinTree

type Machine = Array Int Processor

type Operation = Processor -> IO ()

type Processor = (RAM, RAM, RAM, RAM)

type RAM = BinTree Register

type Register = Array Int Transistor

type Transistor = IORef Bool

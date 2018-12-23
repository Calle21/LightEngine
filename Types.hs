module Types where

import Ubi

type Operation = Processor -> RAM -> Int32 -> IO Signal

type Processor = RAM

type RAM = Array Int Reg

type Reg = IORef Int32

data Signal = Continue | Exit

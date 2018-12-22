module Types where

import Ubi

newtype Executable = Executable RAM

data Mode = Raw | Easy deriving (Enum)

type Operation = Bool -> Mode -> Set -> Pool -> Processor -> RAM -> Int32 -> IO Signal

data Pool = Pool (IORef Int) (Array Int (IORef Processor))

data Processor = Processor Int Regs

type RAM = Array Int Reg

type Reg = IORef Int32

type Regs = Array Int Reg

data Signal = Continue | Exit

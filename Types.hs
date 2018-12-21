module Types where

import Ubi

data Executable = Executable Int RAM

data Mode = Raw | Easy

type Operation = Mode -> Set -> Stack -> Processor -> RAM -> Int32 -> IO Signal

data Processor = Processor Int Regs

type RAM = Array Int Reg

type Reg = IORef Int32

type Regs = Array Int Reg

data Signal = Continue | Exit

data Stack = Stack (IORef Int) (Array Int (IORef Processor))

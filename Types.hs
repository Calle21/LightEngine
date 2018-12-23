module Types where

import Ubi

type Operation = Processor -> RAM -> Int32 -> IO Signal

data Processor = Proc (Array Int Reg) [(Int, IO Int32)]

type RAM = Array Int Reg

type Register = IORef Int32

data Signal = Continue | Return

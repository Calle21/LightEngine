module Types where

type Operation = Processor -> IO ()

type Processor = (RAM, RAM, RAM)

type RAM = Array Int Register

type Register = Array Int (IORef Bool)

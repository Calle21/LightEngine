module Types where

type Operation = Processor -> RAM -> IO ()

type Processor = Array Int Register

type RAM = Array Int Register

type Register = Array Int (IORef Bool)

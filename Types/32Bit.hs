module Types.32Bit where

import qualified Data.Map.Strict as M
import Types
import Ubi
import Util.32Bit (listToArray)

type Operation = Processor -> RAM -> Int32 -> IO Signal

data Processor = Proc Regs (M.Map Int32 (IO Int32))

type Reg = IORef Int32

type Regs = RAM

type RAM = Array Int32 Reg

type Register = IORef Int32

data Signal = Continue | Return Int32

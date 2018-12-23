module Emulator.Operations.Xor where

import Emulator.Operations.Templates (twoOne)
import Types
import Ubi

xor' :: Operation
xor' = twoOne xor

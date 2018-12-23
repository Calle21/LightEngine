module Emulator.Operations.And where

import Emulator.Operations.Templates (twoOne)
import Types
import Ubi

and :: Operation
and = twoOne (.&.)

module Emulator.Operations.Not where

import Emulator.Operations.Templates (oneOne)
import Types
import Ubi

not' :: Operation
not' = oneOne complement

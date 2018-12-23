module Emulator.Operations.Or where

import Emulator.Operations.Templates (twoOne)
import Types
import Ubi

or :: Operation
or = twoOne (.|.)

module Emulator.Operations.Add where

import Emulator.Operations.Templates (twoOne)
import Types

add :: Operation
add = twoOne (+)

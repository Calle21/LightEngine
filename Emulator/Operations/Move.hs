module Emulator.Operations.Move where

import Emulator.Operations.Templates (oneOne)
import Types

move :: Operation
move = oneOne id

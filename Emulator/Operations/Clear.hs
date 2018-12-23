module Emulator.Operations.Clear where

import Emulator.Operations.Templates (one)
import Types

clear :: Operation
clear = one (\_ -> 0)

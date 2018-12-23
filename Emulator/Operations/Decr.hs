module Emulator.Operations.Decr where

import Emulator.Operations.Templates (one)
import Types

decr :: Operation
decr = one pred

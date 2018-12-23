module Emulator.Operations.Incr where

import Emulator.Operations.Templates (one)
import Types

incr :: Operation
incr = one succ

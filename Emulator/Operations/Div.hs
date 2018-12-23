module Emulator.Operations.Div where

import Emulator.Operations.Templates (twoOne)
import Types

div' :: Operation
div' = twoOne div

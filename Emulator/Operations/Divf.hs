module Emulator.Operations.Div where

import Emulator.Operations.Templates (twoOne)
import Types

divf :: Operation
divf = twoOne \i0 i1 -> unsafeCoerce $ (unsafeCoerce i0 :: Float) / (unsafeCoerce i1 :: Float)

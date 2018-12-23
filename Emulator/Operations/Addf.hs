module Emulator.Operations.Addf where

import Emulator.Operations.Templates (twoOne)
import Types

addf :: Operation
addf = twoOne \i0 i1 -> unsafeCoerce $ (unsafeCoerce i0 :: Float) + (unsafeCoerce i1 :: Float)

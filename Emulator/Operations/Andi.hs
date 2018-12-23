module Emulator.Operations.Andi where

import Emulator.Operations.Templates (imm)
import Types
import Ubi

andi :: Operation
andi = imm (.&.)

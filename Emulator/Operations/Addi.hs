module Emulator.Operations.Addi where

import Emulator.Operations.Templates (imm)
import Types
import Ubi
import Util (getReg, unsigned)

addi :: Operation
addi = imm (+)

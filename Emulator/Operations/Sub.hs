module Emulator.Operations.Sub where

import Emulator.Operations.Templates (twoOne)
import Types

sub :: Operation
sub = twoOne (-)

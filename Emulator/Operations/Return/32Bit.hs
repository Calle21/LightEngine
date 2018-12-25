module Emulator.Operations.Return where

import Types.32Bit

return' :: Operation
return' (Proc regs _) _ _ = return Exit

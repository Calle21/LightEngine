module Operations.And where

import Operations.Templates (twoOne)
import Types
import Ubi

and :: Operation
and = twoOne (.&.)

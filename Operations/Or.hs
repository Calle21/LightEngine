module Operations.Or where

import Operations.Templates (twoOne)
import Types
import Ubi

or :: Operation
or = twoOne (.|.)

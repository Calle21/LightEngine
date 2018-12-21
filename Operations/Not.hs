module Operations.Not where

import Operations.Templates (oneOne)
import Types
import Ubi

not' :: Operation
not' = oneOne complement

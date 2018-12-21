module Operations.Add where

import Operations.Templates (twoOne)
import Types

add :: Operation
add = twoOne (+)

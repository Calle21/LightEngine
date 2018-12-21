module Machine where

import Memory
import Types

machine :: IO Machine
machine = (ram 3, ram 13)

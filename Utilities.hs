module Utilities ( safeTail ) where

safeTail :: [a] -> [a]
safeTail []       = []
safeTail (_:tail) = tail

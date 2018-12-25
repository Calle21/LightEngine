module Emulator.Operations.Extras ( adde
                                  , dive
                                  , extra
                                  , mule
                                  , sube ) where

import Ubi

tmpl :: (Int64 -> Int64 -> Int64) -> Int32 -> Int32 -> (Int32, Maybe Int32)
tmpl op v0 v1 = let result = fromIntegral v0 `op` fromIntegral v1
                    extra  = fromIntegral $ result `shiftR` 32
                in (fromIntegral result, Just extra)

adde, dive, mule, sube :: Int32 -> Int32 -> (Int32, Maybe Int32)

adde = tmpl (+)

dive v0 v1 = let (q,r) = divMod v0 v1
             in (q, Just r)

mule = tmpl (*)

sube = tmpl (-)

extra :: (Int32 -> Int32 -> Int32) -> Int32 -> Int32 -> (Int32, Maybe Int32)
extra fn v0 v1 = (fn v0 v1, Nothing)

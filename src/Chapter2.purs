module Chapter2 where

import Math
import Debug.Trace

diagonal w h = sqrt (w * w + h * h)

main = print (diagonal 3 4)
-- main = trace "Hello World!"

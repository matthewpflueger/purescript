module Chapter2 where

import Math
import Debug.Trace

circleArea radius = Math.pi * (radius * radius)
diagonal w h = sqrt (w * w + h * h)

main = print (circleArea 3.4)
-- main = print (diagonal 3 4)
-- main = trace "Hello World!"

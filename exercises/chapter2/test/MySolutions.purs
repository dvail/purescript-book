module Test.MySolutions where

import Prelude

import Global (readFloat)
import Math (pi, sqrt, e)

diagonal :: Number -> Number -> Number
diagonal w h = sqrt (w * w + h * h)

circleArea :: Number -> Number
circleArea radius = radius * radius * pi

addE :: String -> Number
addE stringNum = readFloat(stringNum) + e
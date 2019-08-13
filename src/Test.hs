module Test where

import Markers

test :: Int
test =
  case (underway 1 (underway 0 (solve $ id))) of
    True -> 5
    False -> 0

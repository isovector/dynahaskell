module Test where

import Markers

test :: Int
test = underway 1 (underway 0 (solve) + 5)

module Test where

import Markers

data Fonk a = Fonk Int Bool (a, String)

test :: String -> Bool -> Int -> a -> Fonk a
test = solve


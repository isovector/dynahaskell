{-# OPTIONS_GHC -fdefer-type-errors #-}
module Test where

import Markers

data Fonk a = Fonk Int Bool (a, String)

test :: String -> Int -> (String, Int)
test = solve


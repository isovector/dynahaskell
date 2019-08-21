{-# OPTIONS_GHC -fdefer-type-errors #-}
module Test where

import Markers

data Void
data Fonk a = Fonk Int Bool (a, String)

test :: String -> Either Bool Bool -> (Bool, String)
test = todo 0


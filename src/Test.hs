{-# OPTIONS_GHC -fdefer-type-errors #-}
module Test where

import Markers

data Fonk a = Fonk Int Bool (a, String)

test :: String -> Bool -> Int -> (String, Bool, Int)
test = \a -> \b -> solve


{-# OPTIONS_GHC -fdefer-type-errors #-}
module Test where

import Markers

data Void
data Fonk a = Fonk Int Bool (a, String)

test :: Either String Int -> Bool
test = \x -> todo


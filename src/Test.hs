{-# LANGUAGE RankNTypes #-}

{-# OPTIONS_GHC -fdefer-type-errors #-}

module Test where

import Markers
import Polysemy

data Void
data Fonk a = Fonk Int Bool (a, String)

newtype Cont a = Cont (forall r. (a -> r) -> r)

test :: Maybe Int
test = todo 0

testCont :: (a -> b) -> Cont a -> Cont b
testCont = todo 1


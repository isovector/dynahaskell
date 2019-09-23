{-# LANGUAGE RankNTypes #-}

module Test where

import Markers

data Cont a = Cont (forall r. (a -> r) -> r)

fmapC :: (a -> b) -> Cont a -> Cont b
fmapC f = \ca -> let l = ca in todo 0



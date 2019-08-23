{-# LANGUAGE RankNTypes #-}

{-# OPTIONS_GHC -fdefer-type-errors #-}

module Test where

import Markers

fmapE :: (b -> c) -> Either a b -> Either a c
fmapE = todo 0


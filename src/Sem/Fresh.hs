{-# LANGUAGE TemplateHaskell #-}

module Sem.Fresh where

import Polysemy
import Polysemy.State


data Fresh v m a where
  Fresh :: Fresh v m v

makeSem ''Fresh


runFresh
    :: forall s r a
     . Num s
    => Sem (Fresh s ': r) a
    -> Sem r a
runFresh
  = evalState @s 0
  . reinterpret \case
      Fresh -> do
        i <- get @s
        modify (+1)
        pure i


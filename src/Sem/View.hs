{-# LANGUAGE TemplateHaskell #-}

module Sem.View where

import Polysemy
import Polysemy.Internal
import Polysemy.State
import Data.Coerce


data View v m a where
  View :: View v m v

makeSem ''View


viewToState
    :: forall v s r a
     . Member (State s) r
    => (s -> Sem r v)
    -> Sem (View v ': r) a
    -> Sem r a
viewToState f m = do
  s0 <- get
  v0 <- f s0
  evalState' @v v0
    $ intercept @(State s) @(State' v ': r)
      ( \case
        Get -> get
        Put s -> do
          put s
          s' <- raise $ f s
          send $ State' $ Put s'
      )
    $ reinterpret @(View v) @(State' v)
      ( \case
          View -> send $ State' Get
      )
    $ m

newtype State' s m a = State' (State s m a)

evalState' :: forall s r a. s -> Sem (State' s ': r) a  -> Sem r a
evalState' s = evalState s . reinterpret \case
  State' m -> send $ coerce m


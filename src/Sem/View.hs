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
  evalState' (True, undefined)
    $ intercept @(State s)
      ( \case
        Get -> get
        Put s -> do
          put s
          send $ State' $ Put (True, undefined)
      )
    $ reinterpret @(View v) @(State' (Bool, v))
      ( \case
          View -> do
            (dirty, v) <- send $ State' Get
            case dirty of
              True -> do
                s <- get
                v' <- raise $ f s
                send $ State' $ Put (False, v')
                pure v'
              False -> pure v
      )
    $ m

newtype State' s m a = State' (State s m a)

evalState' :: forall s r a. s -> Sem (State' s ': r) a  -> Sem r a
evalState' s = evalState s . reinterpret \case
  State' m -> send $ coerce m


module Zipper where

import Polysemy
import Polysemy.State

data Zipper a = Zipper [a] a [a]

focusZ :: Zipper a -> a
focusZ (Zipper _ a _) = a

modifyZ :: (a -> a) -> Zipper a -> Zipper a
modifyZ f (Zipper past a _) = Zipper (a : past) (f a) []

goBack :: Zipper a -> Zipper a
goBack z@(Zipper [] _ _) = z
goBack (Zipper (p : ps) a fs) = Zipper ps p (a : fs)

goForward :: Zipper a -> Zipper a
goForward z@(Zipper _ _ []) = z
goForward (Zipper ps a (f : fs)) = Zipper (a : ps) f fs

focus :: Member (State (Zipper a)) r => Sem r a
focus = gets focusZ

record :: Member (State (Zipper a)) r => a -> Sem r ()
record = modify . modifyZ . const

recording :: Member (State (Zipper a)) r => (a -> a) -> Sem r ()
recording = modify . modifyZ

undo :: Member (State (Zipper a)) r => Sem r ()
undo = modify goBack

redo :: Member (State (Zipper a)) r => Sem r ()
redo = modify goForward


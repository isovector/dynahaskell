module Markers where

todo :: Int -> a
todo = error "todo"

solve :: a
solve = error "solve"

underway :: Int -> a -> a
underway = const id


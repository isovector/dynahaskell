module Markers where

inprogress :: a
inprogress = error "in progress"

underway :: Int -> a -> a
underway = error "underway"


{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TupleSections             #-}

module Lib where

import Brick.Main
import Control.Lens (taking)
import Control.Monad
import Data.Maybe
import Data.Monoid
import DynFlags
import MarkerUtils
import Polysemy
import Polysemy.Input
import Polysemy.State
import Polysemy.Trace
import Sem.Anns
import Sem.FileProvider
import Sem.Fresh
import Sem.Ghc
import Sem.HoleInfo
import Sem.Typecheck
import UI
import Zipper
import HIE.Bios



main :: IO ()
main = do
  runGHC "."
       . traceToIO
       . runFresh @Integer
       . runTypechecker
       . stateAndInput (Zipper [] undefined [])
       . runFileProvider
       . runAnno
       $ do
    editFile "src/Test.hs"
    src <- focus

    let l = taking 1 anyTodo
    holes <- holeInfo l src
    void $ defaultMain app $ defData l (listToMaybe holes)


stateAndInput :: s -> Sem (Input s ': State s ': r) a -> Sem r a
stateAndInput s = evalState s . runInputSem get


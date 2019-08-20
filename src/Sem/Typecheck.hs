{-# LANGUAGE TemplateHaskell #-}

module Sem.Typecheck where

import Types
import Polysemy
import GHC
import HIE.Bios
import Language.Haskell.GHC.ExactPrint


data Typecheck m a where
  Typecheck :: Source -> Typecheck m (Maybe TypecheckedModule)

makeSem ''Typecheck


runTypechecker
    :: Members '[Embed Ghc, Embed IO] r
    => Sem (Typecheck ': r) a
    -> Sem r a
runTypechecker = interpret \case
  Typecheck (Source anns lmod) -> do
    let printed = exactPrint lmod anns
    embed $ writeFile "/tmp/dyna.hs" printed
    fmap fst $ embed $ loadFile @Ghc ("/tmp/dyna.hs", "/tmp/dyna.hs")


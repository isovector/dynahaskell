{-# LANGUAGE TemplateHaskell #-}

module Sem.Typecheck where

import Types
import Polysemy
import GHC
import HIE.Bios
import Printers


data Typecheck m a where
  Typecheck :: Source -> Typecheck m (Maybe TypecheckedModule)

makeSem ''Typecheck


runTypechecker
    :: Members '[Embed Ghc, Embed IO] r
    => Sem (Typecheck ': r) a
    -> Sem r a
runTypechecker = interpret \case
  Typecheck src -> do
    embed $ writeFile "/tmp/dyna.hs" $ mconcat
      [ "{-# OPTIONS_GHC -fdefer-type-errors #-}\n"
      , prettySource src
      ]
    fmap (fmap fst) $ embed $ loadFile @Ghc ("/tmp/dyna.hs", "/tmp/dyna.hs")



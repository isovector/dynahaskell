{-# LANGUAGE TemplateHaskell #-}

module Sem.Ghcid where

import Polysemy
import Language.Haskell.Ghcid


data Ghcid m a where
  SetContents  :: String -> Ghcid m ()
  LoadContents :: Ghcid m [String]
  DoEval       :: String -> Ghcid m [String]


makeSem ''Ghcid



runGhcid :: Member (Embed IO) r => Sem (Ghcid ': r) a -> Sem r a
runGhcid m = do
  (g, _) <- embed $ startGhci "stack repl" (Just ".") (const $ const $ pure ())

  z <- interpret
    ( \case
        SetContents msg -> embed $ writeFile "/tmp/dyna.hs" msg
        LoadContents    -> embed $ exec g ":l /tmp/dyna.hs"
        DoEval msg      -> embed $ exec g msg
    ) m

  embed $ stopGhci g
  pure z


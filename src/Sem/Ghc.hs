module Sem.Ghc
  ( module Sem.Ghc
  , Ghc
  ) where

import HIE.Bios.GHCApi
import HIE.Bios.Cradle
import Polysemy
import Polysemy.IO
import GhcMonad


runGHC :: FilePath -> Sem '[Embed IO, Embed Ghc] a -> IO a
runGHC file m = do
  cradle <- findCradle file
  withGHC' $ do
    initializeFlagsWithCradle file cradle
    runM $ embedToMonadIO m


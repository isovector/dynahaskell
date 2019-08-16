module Sem.Ghc
  ( module Sem.Ghc
  , Ghc
  ) where

import HIE.Bios.GHCApi
import HIE.Bios.Cradle
import Polysemy
import Polysemy.IO
import GhcMonad


runGHC :: Sem '[Embed IO, Embed Ghc] a -> IO a
runGHC m = do
  cradle <- findCradle "../test-dyna/src/Test.hs"
  withGHC' $ do
    initializeFlagsWithCradle "../test-dyna/src/Test.hs" cradle
    runM $ embedToMonadIO m


{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}

module Sem.FileProvider where

import DynFlags
import GHC (SrcSpan, tm_typechecked_source)
import Language.Haskell.GHC.ExactPrint.Parsers hiding (parseModuleFromString)
import Polysemy
import Polysemy.State
import Polysemy.Input
import Types
import Zipper
import Printers
import Sem.Ghc
import HIE.Bios.Load (loadFile)


data FileProvider m a where
  EditFile :: FilePath -> FileProvider m ()
  SaveFile :: FileProvider m ()

makeSem ''FileProvider


parseFileModule
  :: FilePath
  -> DynFlags
  -> IO (Either (SrcSpan, String) (DynFlags, Source))
parseFileModule fp dflags = do
  s <- readFile fp
  pure . fmap ((dflags, ) . uncurry Source)
       $ parseModuleFromStringInternal dflags fp s


runFileProvider
    :: Members '[State (Zipper Source), Embed IO, Embed Ghc] r
    => Sem (Input DynFlags ': FileProvider ': State DynFlags ': r) a
    -> Sem r a
runFileProvider
  = evalState @DynFlags undefined
  . evalState ""
  . (reinterpret \case
      EditFile fp -> do
        mdflags <- embed $ loadFile @Ghc (fp, fp)
        dflags <- case mdflags of
          Just (_tcmod, dflags) -> pure dflags
          Nothing -> error "runFileProvider: no dflags"
        put dflags

        z <- embed $ parseFileModule fp dflags
        (_dflags, src) <- case z of
          Left e -> error $ show e
          Right x -> pure x
        put fp
        put $ Zipper [] src []

      SaveFile -> do
        fp <- get
        z <- focus
        embed $ writeFile fp $ prettySource z
    )
  . interpret \case
      Input -> get @DynFlags


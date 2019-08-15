{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}

module Sem.Typecheck where

import Control.Lens
import Control.Monad
import Data.Bifunctor
import Data.List
import Data.Maybe
import Language.Haskell.GHC.ExactPrint
import Language.Haskell.GHC.ExactPrint.Parsers hiding (parseModuleFromString)
import MarkerUtils
import Polysemy
import Polysemy.Input
import Polysemy.State
import Polysemy.Trace
import Printers
import Sem.Ghcid
import Types


data Typecheck m a where
  Typecheck :: Traversal' LModule Expr
            -> Typecheck m Type

makeSem ''Typecheck



holeTypeToGhcid
    :: Members '[ Ghcid
                , State LModule
                , Input Anns
                , Input DynFlags
                , Trace
                ] r
    => Sem (Typecheck ': r) a
    -> Sem r a
holeTypeToGhcid =
  interpret \case
    Typecheck l -> do
      ast  <- get
      anns <- input
      dflags <- input

      when (isNothing $ ast ^? l) $ trace "typechecking lens matched nothing"

      setContents $ uncurry exactPrint $ foo anns $
        ast & l .~ mkHole "_dyna_type"
      cts <- loadContents

      let ft = findHoleType $ concat cts
      (_, L _ t) <- either (const $ error $ concat cts) pure $ getHoleType dflags ft
      pure t


findHoleType :: String -> String
findHoleType = takeUntilP (\s -> isPrefixOf "Or perhaps" s || isPrefixOf "Where: " s)
             . drop (length preamble)
             . dropUntil (isPrefixOf preamble)
  where
    preamble = "Found hole: _dyna_type :: "


getHoleType :: DynFlags -> String -> Either String (Anns, Located (HsType GhcPs))
getHoleType dflags
    = first snd
    . parseType dflags "<dyna>"


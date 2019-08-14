{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}

module Sem.Typecheck where

import Polysemy
import Polysemy.State
import Polysemy.Input
import Polysemy.Trace
import Sem.Ghcid
import GHC
import Language.Haskell.GHC.ExactPrint
import Printers
import Data.List
import Data.Bifunctor
import Language.Haskell.GHC.ExactPrint.Parsers hiding (parseModuleFromString)
import GHC (DynFlags)
import MarkerUtils
import Control.Lens

data Typecheck m a where
  Typecheck :: Traversal' (Located (HsModule GhcPs)) (HsExpr GhcPs)
            -> Typecheck m (Maybe (HsType GhcPs))

makeSem ''Typecheck



holeTypeToGhcid
    :: Members '[ Ghcid
                , State (Located (HsModule GhcPs))
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

      setContents $ uncurry exactPrint $ foo anns $
        ast & l .~ mkHole "_dyna_type"
      cts <- loadContents

      let ft = findHoleType $ concat cts
      case ft of
        "" -> pure Nothing
        _ -> do
          (_, L _ t) <- either (const $ error $ concat cts) pure $ getHoleType dflags ft
          pure $ Just t


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

dropUntil :: ([a] -> Bool) -> [a] -> [a]
dropUntil f s@(_:xs)
  | f s = s
  | otherwise = dropUntil f xs
dropUntil _ [] = []

takeUntilP :: ([a] -> Bool) -> [a] -> [a]
takeUntilP f s@(x:xs)
  | f s = []
  | otherwise = x : takeUntilP f xs
takeUntilP _ [] = []



{-# LANGUAGE TemplateHaskell #-}

module Sem.HoleType where

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

data HoleType m a where
  HoleType :: HoleType m (Maybe (HsType GhcPs))

makeSem ''HoleType



holeTypeToGhcid
    :: Members '[ Ghcid
                , State (Located (HsModule GhcPs))
                , Input Anns
                , Input DynFlags
                , Trace
                ] r
    => Sem (HoleType ': r) a
    -> Sem r a
holeTypeToGhcid =
  interpret \case
    HoleType -> do
      ast  <- get
      anns <- input
      dflags <- input

      setContents $ uncurry exactPrint $ foo anns ast
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
    preamble = "Found hole: _to_solve :: "


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



{-# LANGUAGE TemplateHaskell #-}

module Sem.FillHole where

import MarkerUtils
import Control.Lens
import Polysemy
import Polysemy.State
import Polysemy.Input
import Sem.Ghcid
import GHC (HsExpr, HsModule, Located, GhcPs, DynFlags, unLoc)
import Language.Haskell.GHC.ExactPrint
import Language.Haskell.GHC.ExactPrint.Parsers hiding (parseModuleFromString)


data FillResult = FillOK | BadParse | BadType
  deriving (Eq, Ord, Show, Enum, Bounded)


data FillHole m a where
  FillHole :: String -> FillHole m FillResult

makeSem ''FillHole


runFillHole
    :: Members '[Ghcid, Input DynFlags, State (Located (HsModule GhcPs)), State Anns] r
    => Sem (FillHole ': r) a
    -> Sem r a
runFillHole = interpret \case
  FillHole unparsed -> do
    dflags <- input
    case parseExpr dflags "fillHole" unparsed of
      Left _ -> pure BadParse
      Right (anns, expr) -> do
        modify @(Located (HsModule GhcPs)) $ nowUnderwayC .~ unLoc expr
        modify $ mappend anns
        pure FillOK


{-# LANGUAGE TemplateHaskell #-}

module Sem.FillHole where

import Control.Lens
import Data.Bool
import Language.Haskell.GHC.ExactPrint
import Language.Haskell.GHC.ExactPrint.Parsers hiding (parseModuleFromString)
import MarkerUtils
import Polysemy
import Polysemy.Input
import Polysemy.State
import Sem.Ghcid
import Types


data FillResult = FillOK | BadParse | BadType
  deriving (Eq, Ord, Show, Enum, Bounded)


data FillHole m a where
  FillHole :: String -> FillHole m FillResult

makeSem ''FillHole


runFillHole
    :: Members '[Ghcid, Input DynFlags, State LModule, State Anns] r
    => Sem (FillHole ': r) a
    -> Sem r a
runFillHole = interpret \case
  FillHole unparsed -> do
    let needs_parens = elem ' ' unparsed
    dflags <- input
    case parseExpr dflags "fillHole" unparsed of
      Left _ -> pure BadParse
      Right (anns, expr) -> do
        modify @LModule $
          nowUnderwayC .~ unLoc (bool id (noLoc . HsPar NoExt) needs_parens expr)
        modify $ mappend anns
        pure FillOK


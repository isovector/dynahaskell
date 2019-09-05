{-# LANGUAGE TemplateHaskell #-}

module Sem.API where

import           Control.Monad.Except
import           Polysemy
import           Refinery.Tactic (runProvableT)
import           Sem.Fresh
import           SrcLoc
import qualified Tactics as T
import qualified TacticsV2 as T
import           Types


data EditAPI m a where
  Auto     :: EditAPI m ()
  One      :: EditAPI m ()
  Homo     :: OccName -> EditAPI m ()
  Destruct :: OccName -> EditAPI m ()
  Replace  :: LExpr -> EditAPI m ()

makeSem ''EditAPI


data EditLocation m a where
  GetEditLocation :: EditLocation m SrcSpan

makeSem ''EditLocation


data SrcTree m a where
  SearchTree :: (LExpr -> Bool) -> SrcTree m [SrcSpan]
  GetSpanExpr :: SrcSpan -> SrcTree m LExpr
  ReplaceSpanExpr :: SrcSpan -> LExpr -> SrcTree m ()

makeSem ''SrcTree


data TacticsEngine m a where
  GetJudgementAt :: SrcSpan -> TacticsEngine m T.Judgement
  UpdateJudgements :: [T.Judgement] -> TacticsEngine m ()

makeSem ''TacticsEngine


runEditAPI
    :: forall r
     . Members '[ EditLocation
                , SrcTree
                , Fresh Integer
                , TacticsEngine
                ] r
    => InterpreterOf EditAPI r
runEditAPI = interpret \case
  Auto         -> runTacticOf T.auto
  One          -> runTacticOf T.one
  Homo occ     -> runTacticOf $ T.homo occ
  Destruct occ -> runTacticOf $ T.destruct occ
  Replace ast  -> do
    loc <- getEditLocation
    replaceSpanExpr loc ast

 where
  runTacticOf :: T.Tactic r -> Sem r ()
  runTacticOf t = do
    loc <- getEditLocation
    jdg <- getJudgementAt loc
    z <- runExceptT . runProvableT $ T.runTacticT t jdg
    case z of
      Left _err -> pure ()
      Right (ast, jdgs') -> do
        updateJudgements jdgs'
        replaceSpanExpr loc ast


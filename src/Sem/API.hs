{-# LANGUAGE TemplateHaskell #-}

module Sem.API where

import           Control.Arrow ((***))
import           Control.Lens
import           Control.Monad.Except
import           Data.Foldable
import           Data.Maybe
import           Data.Traversable
import           MarkerUtils
import           Name
import           Polysemy
import           Polysemy.State
import           Refinery.Tactic (runProvableT)
import           Sem.Anns
import           Sem.Fresh
import           Sem.HoleInfo
import           Sem.Typecheck
import           SrcLoc
import qualified Tactics as T
import qualified TacticsV2 as T
import           Types
import           Zipper


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
  GetSpanExpr :: SrcSpan -> SrcTree m (Maybe LExpr)
  ReplaceSpanExpr :: SrcSpan -> LExpr -> SrcTree m ()

makeSem ''SrcTree


data TacticsEngine m a where
  GetJudgementAt :: SrcSpan -> TacticsEngine m (Maybe T.Judgement)
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
  Homo occ     -> runTacticOf $ T.homo occ >> T.auto
  Destruct occ -> runTacticOf $ T.destruct occ
  Replace ast  -> do
    loc <- getEditLocation
    replaceSpanExpr loc ast

 where
  runTacticOf :: T.Tactic r -> Sem r ()
  runTacticOf t = do
    loc <- getEditLocation
    mjdg <- getJudgementAt loc
    for_ mjdg $ \jdg -> do
      z <- runExceptT . runProvableT $ T.runTacticT t jdg
      case z of
        Left _err -> pure ()
        Right (ast, jdgs') -> do
          updateJudgements jdgs'
          replaceSpanExpr loc ast


runTacticsEngine
    :: Members '[ Anno
                , State (Zipper Source)
                , Typecheck
                , Fresh Integer
                ] r
    => InterpreterOf TacticsEngine r
runTacticsEngine = interpret \case
  UpdateJudgements _ -> pure ()
  GetJudgementAt src -> do
    let l :: Traversal' LModule LExpr
        l = taking 1 $ locate $ (== src) . getLoc

    source <- focus
    z <- listToMaybe <$> holeInfo l source
    for z $ \(goal, scope) ->
      pure $ T.Judgement 0 (fmap (nameOccName *** T.CType) scope) $ T.CType goal


runSrcTree :: Members '[State (Zipper Source), Anno] r => InterpreterOf SrcTree r
runSrcTree = interpret \case
  SearchTree p -> do
    z <- focus
    pure $ z ^.. locate p . to getLoc
  GetSpanExpr src -> do
    z <- focus
    pure $ listToMaybe $ z ^.. locate ((== src) . getLoc)
  ReplaceSpanExpr src ast -> do
    z <- focus
    record =<< spliceTree (taking 1 $ locate $ (== src) . getLoc) ast z


runEditLocation :: Members '[SrcTree] r => InterpreterOf EditLocation r
runEditLocation = interpret \case
  GetEditLocation -> do
    let p (Todo _) = True
        p _ = False
    fromMaybe noSrcSpan . listToMaybe <$> searchTree p


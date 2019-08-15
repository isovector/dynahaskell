{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}

module Tactics
  ( tactic
  , auto
  , deepen
  , assumption
  , intro
  , FreshInt (..)
  ) where

import Control.Lens
import Control.Monad.Except
import Data.List
import Language.Haskell.GHC.ExactPrint.Parsers hiding (parseModuleFromString)
import MarkerUtils
import Polysemy
import Polysemy.Input
import Refinery.Tactic
import Types


data Judgement = Judgement [(Var, SType)] SType
  deriving (Eq, Ord, Show)

data TacticError
  = UndefinedHypothesis Var
  | GoalMismatch String SType
  | UnsolvedSubgoals [Judgement]
  deriving (Eq, Ord, Show)


type Tactic r = TacticT Judgement Expr (ProvableT Judgement (ExceptT TacticError (Sem r))) ()

type TacticMems r = Members '[Input FreshInt, Input DynFlags] r

assumption :: Tactic r
assumption = rule $ \(Judgement hy g) ->
  case find ((== g) . snd) hy of
    Just (v, _) -> return $ HsVar NoExt $ noLoc $ getVarName v
    Nothing -> throwError $ GoalMismatch "assumption" g

newtype FreshInt = FreshInt { getFreshInt :: Int }
  deriving (Eq, Ord, Show)

intro :: TacticMems r => String -> Tactic r
intro n = rule $ \(Judgement hy g) ->
  case g of
    (SArrTy a b) -> do
      u <- fmap getFreshInt $ lift $ lift $ lift input
      let vname = n ++ show u
          v = Var vname
      sg <- subgoal $ Judgement ((v, a) : hy) b
      e <- lift $ lift $ lift $ syntactically $ mconcat
             [ "\\"
             , vname
             , " -> "
             , "_a"
             ]

      pure $ subst [("_a", sg)] e
    t -> throwError $ GoalMismatch "intro" t


syntactically
    :: TacticMems r
    => String
    -> Sem r Expr
syntactically str = do
  dflags <- input
  case parseExpr dflags "syntactically" str of
    Left _ -> error $ "you called syntactically badly, on " ++ str
    Right (_, expr) -> do
      pure $ unLoc expr


subst :: [(String, Expr)] -> Expr -> Expr
subst = flip $ foldr $ \(s, e) -> locate (matchOcc s) .~ e


tactic :: TacticMems r => Type -> Tactic r -> Sem r (Maybe Expr)
tactic ty t =
  case toSType ty of
    Just sty -> fmap (fmap fst . hush)
              . runExceptT
              . runProvableT
              . runTacticT t
              $ Judgement [] sty
    Nothing -> pure Nothing


hush :: Either a b -> Maybe b
hush (Right b) = Just b
hush _ = Nothing

auto :: TacticMems r => Tactic r
auto = do
  intro "x" <!> assumption
  auto

deepen :: TacticMems r => Int -> Tactic r
deepen 0 = pure ()
deepen depth = do
  intro "x" <!> assumption <!> pure ()
  deepen $ depth - 1


instance MonadExtract Expr (ProvableT Judgement (ExceptT TacticError (Sem r))) where
  hole = lift $ lift $ pure $ HsVar NoExt $ noLoc $ Unqual $ mkVarOcc "solve"


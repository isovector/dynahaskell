{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}

module Tactics
  ( tactic
  , auto
  , one
  , split
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
import Sem.TypeInfo
import Refinery.Tactic
import Types

import Outputable


data Judgement = Judgement [(Var, SType)] SType
  deriving (Eq, Ord, Show)

data TacticError
  = UndefinedHypothesis Var
  | GoalMismatch String SType
  | UnsolvedSubgoals [Judgement]
  deriving (Eq, Ord, Show)


type Tactics r = TacticT Judgement Expr (ProvableT Judgement (ExceptT TacticError (Sem r)))
type Tactic r = Tactics r ()

type TacticMems r = Members
  '[ Input FreshInt
   , Input DynFlags
   , TypeInfo
   ] r

assumption :: Tactic r
assumption = rule $ \(Judgement hy g) ->
  case find ((== g) . snd) hy of
    Just (v, _) -> return $ HsVar NoExt $ noLoc $ getVarName v
    Nothing -> throwError $ GoalMismatch "assumption" g

newtype FreshInt = FreshInt { getFreshInt :: Int }
  deriving (Eq, Ord, Show)


sem
    :: MonadTrans t
    => Sem r a
    -> t (ProvableT Judgement (ExceptT TacticError (Sem r))) a
sem = lift . lift . lift


intro :: forall r. TacticMems r => String -> Tactic r
intro n = rule $ \(Judgement hy g) ->
  case g of
    (SArrTy a b) -> do
      u <- fmap getFreshInt $ sem input
      let vname = n ++ show u
          v = Var vname
      sg <- subgoal $ Judgement ((v, a) : hy) b
      e <- sem $ syntactically $ mconcat
             [ "\\"
             , vname
             , " -> "
             , "_a"
             ]

      pure $ subst [("_a", sg)] e
    t -> throwError $ GoalMismatch "intro" t


split :: TacticMems r => Tactic r
split = rule $ \(Judgement hy g) ->
  case stypeConApps g of
    Nothing -> throwError $ GoalMismatch "split" g
    Just (tc, sts) -> do
      tci <- sem $ typeInfo tc
      case tciCons tci of
        [dc] -> do
          let args = fmap unLoc . hsConDeclArgTys $ con_args dc
              name = head $ getConNames dc
          -- case traverse toSType args of
            -- Just sts -> do
          do
              sgs <- traverse (subgoal . Judgement hy) sts
              pure $ foldl' (\a -> HsApp NoExt (noLoc a) . noLoc . HsPar NoExt . noLoc)
                            (HsVar NoExt name) sgs
            -- Nothing -> throwError $ GoalMismatch "split" g
        _ -> throwError $ GoalMismatch "split" g



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
    Just sty -> do
      fmap (fmap fst . hush)
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
  intro "x" <!> split <!> assumption
  auto

deepen :: TacticMems r => Int -> Tactic r
deepen 0 = pure ()
deepen depth = do
  one
  deepen $ depth - 1

one :: TacticMems r => Tactic r
one = intro "x" <!> assumption <!> split <!> pure ()


instance MonadExtract Expr (ProvableT Judgement (ExceptT TacticError (Sem r))) where
  hole = lift $ lift $ pure $ HsVar NoExt $ noLoc $ Unqual $ mkVarOcc "solve"


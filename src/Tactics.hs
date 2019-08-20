{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}

module Tactics
  ( tactic
  , auto
  , one
  -- , split
  , deepen
  , assumption
  , intro
  ) where

import Data.List
import Polysemy
import Types
import Language.Haskell.GHC.ExactPrint.Parsers hiding (parseModuleFromString)
import Polysemy.Input
import Refinery.Tactic
import Sem.Fresh
import Data.Maybe
import Control.Lens
import MarkerUtils
import Control.Monad.Except
import TyCoRep
import TcType
import Data.Function

import Outputable

newtype CType = CType { unCType :: Type }

instance Eq CType where
  (==) = eqType `on` unCType

instance Ord CType where
  compare = nonDetCmpType `on` unCType



data Judgement = Judgement [(OccName, CType)] CType
  deriving (Eq, Ord)

data TacticError
  = UndefinedHypothesis OccName
  | GoalMismatch String CType
  | UnsolvedSubgoals [Judgement]


type Tactics r = TacticT Judgement LExpr (ProvableT Judgement (ExceptT TacticError (Sem r)))
type Tactic r = Tactics r ()

type TacticMems r = Members
  '[ Fresh Int
   , Input DynFlags
   ] r

assumption :: Tactic r
assumption = rule $ \(Judgement hy g) ->
  case find ((== g) . snd) hy of
    Just (v, _) -> pure $ noLoc $ HsVar NoExt $ noLoc $ Unqual v
    Nothing -> throwError $ GoalMismatch "assumption" g


sem
    :: MonadTrans t
    => Sem r a
    -> t (ProvableT Judgement (ExceptT TacticError (Sem r))) a
sem = lift . lift . lift


intro :: forall r. TacticMems r => String -> Tactic r
intro n = rule $ \(Judgement hy g) ->
  case unCType g of
    (FunTy a b) -> do
      u <- sem $ fresh @Int
      let vname = n ++ show u
          v = mkVarOcc vname
      sg <- subgoal $ Judgement ((v, CType a) : hy) $ CType b
      e <- sem $ syntactically $ mconcat
             [ "\\"
             , vname
             , " -> "
             , "_a"
             ]

      pure $ substHole [("_a", sg)] e
    _ -> throwError $ GoalMismatch "intro" g


-- split :: TacticMems r => Tactic r
-- split = rule $ \(Judgement hy g) ->
--   case stypeConApps g of
--     _ -> throwError $ GoalMismatch "split" g
--     -- Just (tc, apps) -> do
--     --   tci <- sem $ typeInfo tc
--     --   case tciCons tci of
--     --     [dc] -> do
--     --       let args = fmap unLoc . hsConDeclArgTys $ con_args dc
--     --           subs = zip (tciVars tci) apps
--     --           -- need to substHole the tciVars with apps in this ^
--     --           name = head $ getConNames dc
--     --       case traverse toSType args of
--     --         Just sts -> do
--     --           sgs <- traverse (subgoal . Judgement hy . substTVars subs) sts
--     --           pure $ foldl' (\a -> HsApp NoExt (noLoc a) . parenthesizeHsExpr appPrec . noLoc)
--     --                         (HsVar NoExt name) sgs
--     --         Nothing -> throwError $ GoalMismatch "split" g
--     --     _ -> throwError $ GoalMismatch "split" g



syntactically
    :: TacticMems r
    => String
    -> Sem r LExpr
syntactically str = do
  dflags <- input
  case parseExpr dflags "syntactically" str of
    Left _ -> error $ "you called syntactically badly, on " ++ str
    Right (_, expr) -> do
      pure expr

-- -- substTVars :: [(TVar, Type)] -> Type -> Type
-- -- substTVars subs (STyVar t) = fromMaybe (error "substTVars") $ lookup t subs
-- -- substTVars _ t@(STyCon _) = t
-- -- substTVars subs (SArrTy a b) = SArrTy (substTVars subs a) (substTVars subs b)
-- -- substTVars subs (SAppTy a b) = SAppTy (substTVars subs a) (substTVars subs b)

substHole :: [(String, LExpr)] -> LExpr -> LExpr
substHole = flip $ foldr $ \(s, e) -> locate (matchOcc s) .~ e


tactic :: TacticMems r => Type -> Tactic r -> Sem r (Maybe LExpr)
tactic ty t =
      fmap (fmap fst . hush)
              . runExceptT
              . runProvableT
              . runTacticT t
              $ Judgement [] $ CType ty


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
  one
  deepen $ depth - 1

one :: TacticMems r => Tactic r
one = intro "x" <!> assumption <!> pure ()


instance MonadExtract LExpr (ProvableT Judgement (ExceptT TacticError (Sem r))) where
  hole = lift $ lift $ pure $ noLoc $ HsVar NoExt $ noLoc $ Unqual $ mkVarOcc "solve"


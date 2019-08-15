{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}

module Tactics where

import Data.Char
import Control.Applicative
import Refinery.Tactic
import GHC.Exts
import Polysemy
import Control.Monad.Trans.Except
import Control.Monad.Except
import Data.List
import Types
import OccName
import Polysemy.Input

newtype Var = Var String
  deriving (Eq, Ord, Show, IsString)

newtype TVar = TVar String
  deriving (Eq, Ord, Show, IsString)


data SType
  = STyVar TVar
  | STyCon String
  | SAppTy SType SType
  | SArrTy SType SType
  deriving (Eq, Ord, Show)


getVarName :: Var -> IdP GhcPs
getVarName (Var v) = Unqual $ mkVarOcc v


toSType :: Type -> Maybe SType
toSType (HsTyVar _ NotPromoted (L _ (Unqual (occNameString -> t)))) =
  case isUpper $ head t of
    True ->  Just $ STyCon t
    False -> Just $ STyVar $ TVar t
toSType (HsTupleTy _ HsBoxedTuple a) =
  let l = length a
   in foldl' (\s -> liftA2 SAppTy s . toSType . unLoc)
             (Just $ STyCon $ replicate l ',') a
toSType (HsForAllTy _ _ (L _ t))    = toSType t
toSType (HsTyVar _ _ _)             = Nothing
toSType (HsQualTy _ _ _)            = Nothing
toSType (HsAppTy _ (L _ a) (L _ b)) = SAppTy <$> toSType a <*> toSType b
toSType (HsFunTy _ (L _ a) (L _ b)) = SArrTy <$> toSType a <*> toSType b
toSType (HsListTy _ (L _ a))        = SAppTy <$> pure (STyCon "[]") <*> toSType a
toSType (HsTupleTy _ _ _)           = Nothing
toSType (HsSumTy _ _)               = Nothing
toSType (HsOpTy _ _ _ _)            = Nothing -- TODO(sandy): do this tho
toSType (HsParTy _ (L _ a))         = toSType a
toSType (HsIParamTy _ _ _)          = Nothing
toSType (HsStarTy _ _)              = Just $ STyCon "*"
toSType (HsKindSig _ (L _ a) _)     = toSType a
toSType (HsSpliceTy _ _)            = Nothing
toSType (HsDocTy _ _ _)             = Nothing
toSType (HsBangTy _ _ _)            = Nothing
toSType (HsRecTy _ _)               = Nothing
toSType (HsExplicitListTy _ _ _)    = Nothing
toSType (HsExplicitTupleTy _ _)     = Nothing
toSType (HsTyLit _ _)               = Nothing
toSType (HsWildCardTy _)            = Nothing
toSType (XHsType _)                 = Nothing


fromSType :: SType -> Type
fromSType (STyVar (TVar t)) =
  HsTyVar NoExt NotPromoted $ noLoc $ Unqual $ mkVarOcc t
fromSType (STyCon t) =
  HsTyVar NoExt NotPromoted $ noLoc $ Unqual $ mkVarOcc t
fromSType (SAppTy a b) =
  HsAppTy NoExt (noLoc $ fromSType a) (noLoc $ fromSType b)
fromSType (SArrTy a b) =
  HsFunTy NoExt (noLoc $ fromSType a) (noLoc $ fromSType b)


data Judgement = Judgement [(Var, SType)] SType
  deriving (Eq, Ord, Show)

data TacticError
  = UndefinedHypothesis Var
  | GoalMismatch String SType
  | UnsolvedSubgoals [Judgement]
  deriving (Eq, Ord, Show)


type Tactic r = TacticT Judgement Expr (ProvableT Judgement (ExceptT TacticError (Sem r))) ()

assumption :: Tactic r
assumption = rule $ \(Judgement hy g) ->
  case find ((== g) . snd) hy of
    Just (v, _) -> return $ HsVar NoExt $ noLoc $ getVarName v
    Nothing -> throwError $ GoalMismatch "assumption" g

newtype FreshInt = FreshInt { getFreshInt :: Int }
  deriving (Eq, Ord, Show)

intro :: forall r. Member (Input FreshInt) r => String -> Tactic r
intro n = rule $ \(Judgement hy g) ->
  case g of
    (SArrTy a b) -> do
      u <- fmap getFreshInt $ lift $ lift $ lift input
      let v = Var $ n ++ show u
      sg <- subgoal $ Judgement ((v, a) : hy) b
      pure $ HsLam NoExt
           $ MG NoExt
                ( noLoc
                . pure
                . noLoc
                . Match NoExt LambdaExpr (pure . noLoc $ VarPat NoExt $ noLoc $ getVarName v)
                . GRHSs NoExt (pure $ noLoc $ GRHS NoExt [] $ noLoc sg)
                . noLoc
                $ emptyLocalBinds
                )
                FromSource
    t -> throwError $ GoalMismatch "intro" t

instance MonadExtract Expr (ProvableT Judgement (ExceptT TacticError (Sem r))) where
  hole = lift $ lift $ pure $ HsVar NoExt $ noLoc $ Unqual $ mkVarOcc "todo"


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
import MarkerUtils
import Language.Haskell.GHC.ExactPrint.Parsers hiding (parseModuleFromString)
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
toSType (HsTupleTy _ _ a) =
  let l = length a
   in foldl' (\s -> liftA2 SAppTy s . toSType . unLoc)
             (Just . STyCon $ ('(':)
                   . (++ ")")
                   $ replicate l ',') a
toSType (HsForAllTy _ _ (L _ t))    = toSType t
toSType (HsTyVar _ _ _)             = Nothing
toSType (HsQualTy _ _ _)            = Nothing
toSType (HsAppTy _ (L _ a) (L _ b)) = SAppTy <$> toSType a <*> toSType b
toSType (HsFunTy _ (L _ a) (L _ b)) = SArrTy <$> toSType a <*> toSType b
toSType (HsListTy _ (L _ a))        = SAppTy <$> pure (STyCon "[]") <*> toSType a
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


_fromSType :: SType -> Type
_fromSType (STyVar (TVar t)) =
  HsTyVar NoExt NotPromoted $ noLoc $ Unqual $ mkVarOcc t
_fromSType (STyCon t) =
  HsTyVar NoExt NotPromoted $ noLoc $ Unqual $ mkVarOcc t
_fromSType (SAppTy a b) =
  HsAppTy NoExt (noLoc $ _fromSType a) (noLoc $ _fromSType b)
_fromSType (SArrTy a b) =
  HsFunTy NoExt (noLoc $ _fromSType a) (noLoc $ _fromSType b)


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


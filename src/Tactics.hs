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
  , destruct
  ) where

import Control.Lens
import Control.Monad.Except
import Data.Bifunctor
import Data.Function
import Data.List
import Data.Traversable
import DataCon
import Language.Haskell.GHC.ExactPrint.Parsers hiding (parseModuleFromString)
import MarkerUtils
import Name hiding (varName)
import Outputable
import Polysemy
import Polysemy.Input
import Refinery.Tactic
import Sem.Fresh
import TyCoRep
import TyCon
import Type
import Types

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
type Rule r = RuleT Judgement LExpr (ProvableT Judgement (ExceptT TacticError (Sem r))) LExpr

type TacticMems r = Members
  '[ Fresh Integer
   , Input DynFlags
   ] r

assumption :: Tactic r
assumption = rule $ \(Judgement hy g) ->
  case find ((== g) . snd) hy of
    Just (v, _) -> pure $ noLoc $ HsVar NoExt $ noLoc $ Unqual v
    Nothing -> throwError $ GoalMismatch "assumption" g

-- nice

sem
    :: MonadTrans t
    => Sem r a
    -> t (ProvableT Judgement (ExceptT TacticError (Sem r))) a
sem = lift . lift . lift


intro :: forall r. TacticMems r => String -> Tactic r
intro n = rule $ \(Judgement hy g) ->
  case unCType g of
    (FunTy a b) -> do
      u <- sem $ fresh
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


mkName
    :: Member (Fresh Integer) r
    => MonadTrans t
    => String
    -> t (ProvableT Judgement (ExceptT TacticError (Sem r))) OccName
mkName name = sem $ do
  u <- fresh
  pure $ mkVarOcc $ name ++ show u



destruct :: TacticMems r => OccName -> Tactic r
destruct term = rule $ \(Judgement hy g) -> do
  case find ((== term) . fst) hy of
    Nothing -> throwError $ UndefinedHypothesis term
    Just (_, t) ->
      case splitTyConApp_maybe $ unCType t of
        Nothing -> throwError $ GoalMismatch "destruct" g
        Just (tc, apps) -> do
          fmap noLoc
              $ HsCase NoExt (noLoc $ HsVar NoExt $ noLoc $ Unqual term)
              . flip (MG NoExt) FromSource
              . noLoc <$> do
            for (tyConDataCons tc) $ \dc -> do
              let args = dataConInstArgTys dc apps
              names <- for args $ const $ mkName "c"

              let pat :: Pat GhcPs
                  pat = ConPatIn (noLoc $ Unqual $ nameOccName $ dataConName dc) $ PrefixCon $ do
                          n <- names
                          pure $ noLoc $ VarPat NoExt . noLoc $ Unqual n

              sg <- subgoal $ Judgement (zip names (fmap CType args) ++ hy) g
              pure
                $ noLoc
                $ Match NoExt CaseAlt [noLoc pat]
                $ GRHSs NoExt [noLoc $ GRHS NoExt [] sg]
                $ noLoc
                $ EmptyLocalBinds NoExt


split :: TacticMems r => Tactic r
split = rule $ \(Judgement hy g) ->
  case splitTyConApp_maybe $ unCType g of
    Just (tc, apps) ->
      case tyConDataCons tc of
        [dc] -> buildDataCon hy dc apps
        _ -> throwError $ GoalMismatch "split" g
    Nothing -> throwError $ GoalMismatch "split" g


buildDataCon :: [(OccName, CType)] -> DataCon -> [Type] -> Rule r
buildDataCon hy dc apps = do
  let args = dataConInstArgTys dc apps
  -- pprTraceM "looking for" $ vcat
  --   [ ppr args
  --   , text "in"
  --   , ppr $ fmap (second unCType) hy
  --   ]
  sgs <- traverse (subgoal . Judgement hy . CType) args
  pure . noLoc
       . foldl' (\a -> HsApp NoExt (noLoc a) . parenthesizeHsExpr appPrec)
                (HsVar NoExt $ noLoc $ Unqual $ nameOccName $ dataConName dc)
       $ sgs



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

substHole :: [(String, LExpr)] -> LExpr -> LExpr
substHole = flip $ foldr $ \(s, e) -> locate (matchOcc s) .~ e


tactic :: TacticMems r => Type -> [(OccName, Type)] -> Tactic r -> Sem r (Maybe LExpr)
tactic ty hy t = do
  -- pprTraceM "hy" $ ppr hy
  fmap (fmap fst . hush)
    . runExceptT
    . runProvableT
    . runTacticT t
    . Judgement (fmap (second CType) hy)
    $ CType ty


hush :: Either a b -> Maybe b
hush (Right b) = Just b
hush _ = Nothing

auto :: TacticMems r => Tactic r
auto = do
  intro "x" <!> assumption <!> split
  auto

deepen :: TacticMems r => Int -> Tactic r
deepen 0 = pure ()
deepen depth = do
  one
  deepen $ depth - 1

one :: TacticMems r => Tactic r
one = intro "x" <!> assumption <!> split <!> pure ()


instance Member (Fresh Integer) r
      => MonadExtract LExpr (ProvableT Judgement (ExceptT TacticError (Sem r))) where
  hole = lift $ lift $ do
    i <- fresh
    pure $ Todo i


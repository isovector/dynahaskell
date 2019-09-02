{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}

module Tactics
  ( tactic
  , auto
  , apply
  , one
  , split
  , deepen
  , assumption
  , intro
  , destruct
  , dobodyblock
  , dobindblock
  , homo
  , Tactic
  ) where

import Control.Lens hiding (at)
import Control.Monad.Except
import Control.Monad.State
import Data.Bifunctor
import Data.Char
import Data.List
import Data.Traversable
import DataCon
import HsExprUtils
import Language.Haskell.GHC.ExactPrint.Parsers hiding (parseModuleFromString)
import MarkerUtils
import Name hiding (varName)
import Outputable
import Polysemy
import Polysemy.Input
import Refinery.Tactic hiding (runTacticT)
import Sem.Fresh
import TacticsV2
import TcType (tcSplitSigmaTy, tcSplitFunTys)
import TyCoRep
import TyCon
import Type
import Types



type TacticMems r = Members
  '[ Fresh Integer
   , Input DynFlags
   ] r

assumption :: Tactic r
assumption = rule $ \(Judgement _ hy g) ->
  case find ((== g) . snd) hy of
    Just (v, _) -> pure $ noLoc $ HsVar NoExt $ noLoc $ Unqual v
    Nothing -> throwError $ GoalMismatch "assumption" g



intro :: forall r. TacticMems r => Tactic r
intro = rule $ \(Judgement _ hy g) ->
  case unCType g of
    (FunTy a b) -> do
      v <- sem $ mkGoodName (getInScope hy) a
      let vname = occNameString v
      sg <- newSubgoal ((v, CType a) : hy) $ CType b
      e <- sem $ syntactically $ mconcat
             [ "\\"
             , vname
             , " -> "
             , "_a"
             ]

      pure $ substHole [("_a", sg)] e
    _ -> throwError $ GoalMismatch "intro" g


getInScope :: [(OccName, a)] -> [OccName]
getInScope = fmap fst

destruct' :: TacticMems r => (DataCon -> Judgement -> Rule r) -> OccName -> Tactic r
destruct' f term = rule $ \(Judgement _ hy g) -> do
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
              names <- flip evalStateT (getInScope hy) $ for args $ \at -> do
                in_scope <- Control.Monad.State.get
                n <- lift $ sem $ mkGoodName in_scope at
                Control.Monad.State.modify (n :)
                pure n

              let pat :: Pat GhcPs
                  pat = ConPatIn (noLoc $ Unqual $ nameOccName $ dataConName dc) $ PrefixCon $ do
                          n <- names
                          pure $ noLoc $ VarPat NoExt . noLoc $ Unqual n

              j <- newJudgement (zip names (fmap CType args) ++ hy) g
              sg <- f dc j
              pure
                $ noLoc
                $ Match NoExt CaseAlt [noLoc pat]
                $ GRHSs NoExt [noLoc $ GRHS NoExt [] sg]
                $ noLoc
                $ EmptyLocalBinds NoExt


destruct :: TacticMems r => OccName -> Tactic r
destruct = destruct' $ const subgoal

homo :: TacticMems r => OccName -> Tactic r
homo = destruct' $ \dc (Judgement _ hy (CType g)) -> buildDataCon hy dc (snd $ splitAppTys g)


dobodyblock :: TacticMems r => Tactic r
dobodyblock = rule $ \(Judgement _ hy g@(CType t)) -> do
  case isItAMonad t of
    False -> throwError $ GoalMismatch "dobodyblock" g
    True -> do
      i <- sem fresh
      sg <- newSubgoal hy g
      pure $ Underway i
           $ noLoc
           $ HsDo noExt DoExpr
           $ noLoc
           $ pure
           $ buildBodyStmt sg


dobindblock :: TacticMems r => String -> Tactic r
dobindblock nm = rule $ \(Judgement _ hy g@(CType t)) -> do
  case isItAMonad t of
    False -> throwError $ GoalMismatch "dobindblock" g
    True -> do
      i <- sem fresh
      sg1 <- newSubgoal hy g
      -- TODO(sandy): this is wrong; sg2 should have sg1 bound at nm
      sg2 <- newSubgoal hy g
      pure $ Underway i
           $ noLoc
           $ HsDo noExt DoExpr
           $ noLoc
             [ buildBindStmt nm sg1
             , buildBodyStmt sg2
             ]


isItAMonad :: Type -> Bool
isItAMonad (splitAppTys -> (_, t@(_:_))) = not $ isFunTy $ typeKind $ last t
isItAMonad _ = False


apply :: TacticMems r => Tactic r
apply = rule $ \(Judgement _ hy g) -> do
  case find ((== Just g) . fmap (CType . snd) . splitFunTy_maybe . unCType . snd) hy of
    Just (func, CType ty) -> do
      let (args, _) = splitFunTys ty
      sgs <- traverse (newSubgoal hy . CType) args
      pure . noLoc
           $ foldl' (\a -> HsApp NoExt (noLoc a) . parenthesizeHsExpr appPrec)
                    (HsVar NoExt $ noLoc $ Unqual func) sgs
    Nothing -> throwError $ GoalMismatch "apply" g


split :: TacticMems r => Tactic r
split = rule $ \(Judgement _ hy g) ->
  case splitTyConApp_maybe $ unCType g of
    Just (tc, apps) ->
      case tyConDataCons tc of
        [dc] -> buildDataCon hy dc apps
        _ -> throwError $ GoalMismatch "split" g
    Nothing -> throwError $ GoalMismatch "split" g


buildDataCon :: Member (Fresh Integer) r => [(OccName, CType)] -> DataCon -> [Type] -> Rule r
buildDataCon hy dc apps = do
  let args = dataConInstArgTys dc apps
  sgs <- traverse (newSubgoal hy . CType) args
  pure . noLoc
       . foldl' (\a -> HsApp NoExt (noLoc a) . parenthesizeHsExpr appPrec)
                (HsVar NoExt $ noLoc $ Unqual $ nameOccName $ dataConName dc)
       $ sgs


mkGoodName :: TacticMems r => [OccName] -> Type -> Sem r OccName
mkGoodName in_scope t = do
  let tn = mkTyName t
  fmap mkVarOcc $ case elem (mkVarOcc tn) in_scope of
    True -> do
      i <- fresh
      pure $ tn ++ show i
    False ->
      pure $ tn


mkTyName :: Type -> String
mkTyName (tcSplitFunTys -> ([a@(isFunTy -> False)], b)) = "f" ++ mkTyName a ++ mkTyName b
mkTyName (tcSplitFunTys -> ((_:_), b))                  = "f_" ++ mkTyName b
mkTyName (splitTyConApp_maybe -> Just (c, args))        = mkTyConName c ++ foldMap mkTyName args
mkTyName (getTyVar_maybe-> Just tv)                     = occNameString $ occName tv
mkTyName (tcSplitSigmaTy-> ((_:_), _, t))               = mkTyName t
mkTyName _ = "x"


mkTyConName :: TyCon -> String
mkTyConName = fmap toLower . take 1 . occNameString . getOccName



syntactically
    :: TacticMems r
    => String
    -> Sem r LExpr
syntactically str = do
  i <- fresh
  dflags <- input
  case parseExpr dflags ("syntactically+" ++ show i) str of
    Left _ -> error $ "you called syntactically badly, on " ++ str
    Right (_, expr) -> do
      pure expr

substHole :: [(String, LExpr)] -> LExpr -> LExpr
substHole = flip $ foldr $ \(s, e) -> locate (matchOcc s) .~ e


tactic :: TacticMems r => Type -> [(OccName, Type)] -> Tactic r -> Sem r (Maybe LExpr)
tactic ty hy t = do
  -- TODO(sandy): get a real id here
  i <- fresh
  -- pprTraceM "hy" $ ppr hy
  fmap (fmap fst . hush)
    . runExceptT
    . runProvableT
    . runTacticT t
    . Judgement i (fmap (second CType) hy)
    $ CType ty


deepen :: TacticMems r => Int -> Tactic r
deepen 0 = pure ()
deepen depth = do
  one
  deepen $ depth - 1

auto :: TacticMems r => Tactic r
auto = do
  (intro >> auto) <!> (assumption >> auto) <!> (split >> auto) <!> (apply >> auto) <!> pure ()

one :: TacticMems r => Tactic r
one = intro <!> assumption <!> split <!> apply <!> pure ()


hush :: Outputable a => Either a b -> Maybe b
hush (Right b) = Just b
hush (Left _a) = do
  -- pprTraceM "hushed" $ ppr _a
  Nothing


{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types
  ( module Types
  , module HsSyn
  , module HsTypes
  , Located
  , GenLocated (..)
  , noLoc
  , unLoc
  , DynFlags
  , OccName (..)
  , mkVarOcc
  , RdrName (..)
  , _Ctor'
  , position
  , field
  , field'
  , module BasicTypes
  , Anns
  ) where

import BasicTypes
import Control.Applicative
import Data.Char
import Data.Generics.Product.Positions
import Data.Generics.Sum.Constructors
import Data.Generics.Product.Fields
import Data.List
import GHC hiding (Type)
import GHC.Exts
import HsSyn
import HsTypes
import OccName
import RdrName
import Language.Haskell.GHC.ExactPrint

type Type = HsType GhcPs
type Expr = HsExpr GhcPs
type Module = HsModule GhcPs
type TyDecl = TyClDecl GhcPs

type LType = Located (HsType GhcPs)
type LExpr = Located (HsExpr GhcPs)
type LModule = Located (HsModule GhcPs)


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
  let l = length a - 1
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

stypeConApps :: SType -> Maybe (String, [SType])
stypeConApps = go []
  where
    go zs (STyCon s) = Just (s, zs)
    go zs (SAppTy a b) = go (b : zs) a
    go _ _ = Nothing


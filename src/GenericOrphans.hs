{-# OPTIONS_GHC -fno-warn-orphans #-}

module GenericOrphans where

import Language.Haskell.GHC.ExactPrint.Types (Annotation (..))
import BasicTypes
import GHC.Generics
import HsSyn
import SrcLoc
import RdrName

deriving instance Generic (HsWildCardBndrs GhcPs t)
deriving instance Generic (HsExpr GhcPs)
deriving instance Generic (HsDecl GhcPs)
deriving instance Generic (ConDecl GhcPs)
deriving instance Generic (TyClDecl GhcPs)
deriving instance Generic RdrName
deriving instance Generic (GenLocated l e)
deriving instance Generic (HsOverLit GhcPs)
deriving instance Generic (HsLit GhcPs)
deriving instance Generic (HsModule GhcPs)
deriving instance Generic IntegralLit
deriving instance Generic OverLitVal

deriving instance Generic (HsWildCardBndrs GhcTc t)
deriving instance Generic (HsExpr GhcTc)
deriving instance Generic (HsDecl GhcTc)
deriving instance Generic (ConDecl GhcTc)
deriving instance Generic (TyClDecl GhcTc)
deriving instance Generic (HsOverLit GhcTc)
deriving instance Generic (HsLit GhcTc)
deriving instance Generic (HsModule GhcTc)

deriving instance Generic Annotation

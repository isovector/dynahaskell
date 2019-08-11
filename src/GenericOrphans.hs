{-# OPTIONS_GHC -fno-warn-orphans #-}

module GenericOrphans where

import BasicTypes
import GHC.Generics
import HsSyn
import SrcLoc

deriving instance Generic (HsExpr GhcPs)
deriving instance Generic (GenLocated l e)
deriving instance Generic (HsOverLit GhcPs)
deriving instance Generic (HsLit GhcPs)
deriving instance Generic IntegralLit
deriving instance Generic OverLitVal

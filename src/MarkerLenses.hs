module MarkerLenses where

import GHC.Generics
import HsSyn
import SrcLoc
import Control.Lens
import Data.Generics.Product.Positions
import Data.Generics.Sum.Constructors
import GenericOrphans ()

loc :: Lens' (Located l) l
loc = position @2

appF :: Traversal' (HsExpr GhcPs) (HsExpr GhcPs)
appF = _Ctor' @"HsApp" . position @2 . loc

appA :: Traversal' (HsExpr GhcPs) (HsExpr GhcPs)
appA = _Ctor' @"HsApp" . position @3 . loc

intLit :: Traversal' (HsExpr GhcPs) Integer
intLit = _Ctor' @"HsOverLit"
       . position @2
       . _Ctor' @"OverLit"
       . position @2
       . _Ctor' @"HsIntegral"
       . position @3

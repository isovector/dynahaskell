module MarkerLenses where

import GHC.Generics
import Control.Lens
import GenericOrphans ()
import Types

loc :: Lens' (Located l) l
loc = position @2

appF :: Traversal' Expr Expr
appF = _Ctor' @"HsApp" . position @2 . loc

appA :: Traversal' Expr Expr
appA = _Ctor' @"HsApp" . position @3 . loc

intLit :: Traversal' Expr Integer
intLit = _Ctor' @"HsOverLit"
       . position @2
       . _Ctor' @"OverLit"
       . position @2
       . _Ctor' @"HsIntegral"
       . position @3


{-# LANGUAGE RankNTypes #-}

module MarkerUtils where

import Generics.SYB hiding (Generic)
import Control.Lens
import Data.Data.Lens
import GenericOrphans ()
import MarkerLenses
import Types
import Name hiding (varName)
import Var


matchOcc :: String -> LExpr -> Bool
matchOcc occ (L _ (HsVar _ (L _ (Unqual occ')))) = mkVarOcc occ == occ'
matchOcc _ _ = False

matchVar :: String -> HsExpr GhcTc -> Bool
matchVar occ (HsVar _ (L _ var)) = occNameString (nameOccName (varName var)) == occ
matchVar _ _ = False

locate :: (Data a, Data b) => (b -> Bool) -> Traversal' a b
locate f = biplate . deepOf uniplate (taking 1 $ filtered f)

nextTodo :: Data a => Traversal' a LExpr
nextTodo = locate $ matchOcc "todo"

mkHole :: String -> Expr
mkHole = HsVar NoExt . noLoc . Unqual . mkVarOcc


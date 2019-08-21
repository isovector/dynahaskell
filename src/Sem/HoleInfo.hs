{-# LANGUAGE TemplateHaskell #-}

module Sem.HoleInfo where

import Bag
import Control.Arrow
import Control.Lens
import GHC
import Id
import MarkerLenses
import MarkerUtils
import Polysemy
import Sem.Anns
import Sem.Fresh
import Sem.Typecheck
import TcRnTypes
import Types hiding (Type)


holeInfo
    :: Members '[Typecheck, Anno, Fresh Integer] r
    => Traversal' LModule LExpr
    -> Source
    -> Sem r [(Type, [(Name, Type)])]
holeInfo l t = do
  i <- fresh
  let hole = mkVar $ "_hole" ++ show i
  v <- typecheck =<< spliceTree l (noLoc hole) t
  pure $ case v of
    Nothing -> []
    Just tc -> holeInfoForName ("_hole" ++ show i) tc

getBinderType :: TcBinder -> [(Name, Kind)]
getBinderType (TcIdBndr b _) = pure $ (idName b, idType b)
getBinderType (TcTvBndr _ _)  = []
getBinderType _  = error "getBinderType: this should never happen"


holeInfoForName :: String -> TypecheckedModule -> [(Type, [(Name, Type)])]
holeInfoForName n tc =
  let hole_info = bagToList (tm_typechecked_source tc)
              ^.. locate (matchVar n)
                . _Ctor' @"HsVar"
                . to (id &&& id)
                . alongside (position @2 . loc . to idType)
                            (position @1)
   in fmap (second (>>= getBinderType)) hole_info


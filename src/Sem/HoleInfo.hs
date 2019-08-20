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
import Polysemy.State
import Sem.Anns
import Sem.Fresh
import Sem.Typecheck
import TcRnTypes
import Types hiding (Type)

data HoleInfo m a where
  HoleInfo :: Traversal' LModule LExpr -> HoleInfo m [(Type, [(Name, Type)])]

makeSem ''HoleInfo


runHoleInfo
    :: Members '[Typecheck, State Source, Anno, Fresh Integer] r
    => Sem (HoleInfo ': r) a
    -> Sem r a
runHoleInfo = interpret \case
  HoleInfo l -> do
    i <- fresh
    let hole = mkVar $ "_hole" ++ show i
    t <- get
    v <- typecheck =<< spliceTree l (noLoc hole) t
    put t *>
      case v of
        Nothing -> pure []
        Just tc -> do
          let hole_info = bagToList (tm_typechecked_source tc)
                      ^.. locate (matchVar $ "_hole" ++ show i)
                        . _Ctor' @"HsVar"
                        . to (id &&& id)
                        . alongside (position @2 . loc . to idType)
                                    (position @1)
          pure $ fmap (second $ fmap getBinderType) hole_info

getBinderType :: TcBinder -> (Name, Kind)
getBinderType (TcIdBndr b _) = (idName b, idType b)
getBinderType _ = error "getBinderType: this should never happen"


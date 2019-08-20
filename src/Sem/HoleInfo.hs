{-# LANGUAGE TemplateHaskell #-}

module Sem.HoleInfo where

import Control.Arrow
import MarkerUtils
import Control.Lens
import Polysemy
import Types hiding (Type)
import TcRnTypes
import Sem.Anns
import MarkerLenses
import Polysemy.State
import GHC
import Sem.View
import Id
import Bag

data HoleInfo m a where
  HoleInfo :: Traversal' LModule LExpr -> HoleInfo m [(Type, [(Name, Type)])]

makeSem ''HoleInfo


runHoleInfo
    :: Members '[View (Maybe TypecheckedModule), State LModule, Anno] r
    => Sem (HoleInfo ': r) a
    -> Sem r a
runHoleInfo = interpret \case
  HoleInfo l -> do
    let hole = mkHole "_hole"
    t <- get
    spliceTree l $ noLoc hole
    v <- see
    put t *>
      case v of
        Nothing -> pure []
        Just tc -> do
          let hole_info = bagToList (tm_typechecked_source tc)
                      ^.. locate (matchVar "_hole")
                        . _Ctor' @"HsVar"
                        . to (id &&& id)
                        . alongside (position @2 . loc . to idType)
                                    (position @1)
          pure $ fmap (second $ fmap getBinderType) hole_info

getBinderType :: TcBinder -> (Name, Kind)
getBinderType (TcIdBndr b _) = (idName b, idType b)
getBinderType _ = error "getBinderType: this should never happen"


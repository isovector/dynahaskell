{-# LANGUAGE TemplateHaskell #-}

module Sem.TypeInfo where

import Data.List
import Language.Haskell.GHC.ExactPrint.Parsers hiding (parseModuleFromString)
import Polysemy
import Polysemy.Input
import Sem.Ghcid
import Types


data TCInfo = TCInfo
  { tcName :: RdrName
  , tcVars :: [HsTyVarBndr GhcPs]
  , tcCons :: [ConDecl GhcPs]
  }


data TypeInfo m a where
  TypeInfo :: String -> TypeInfo m TCInfo

makeSem ''TypeInfo


runTypeInfo :: Members '[Ghcid, Input DynFlags] r => Sem (TypeInfo ': r) a -> Sem r a
runTypeInfo =
  interpret \case
    TypeInfo tycon -> do
      dflags <- input

      c <- doEval $ ":info " ++ tycon
      let d = ("data " ++)
            . takeUntilP (isPrefixOf "-- Defined")
            . dropUntil (isPrefixOf tycon)
            $ concat c

      (_, z)
        <- either (error . snd) pure
         $ parseDecl dflags "<typeinfo>" d
      let L _ (TyClD _ (DataDecl _ (L _ tcname) (HsQTvs _ tyvars) _ (HsDataDefn _ _ _ _ _ cs _))) = z

      pure $ TCInfo tcname (fmap unLoc tyvars) $ fmap unLoc cs




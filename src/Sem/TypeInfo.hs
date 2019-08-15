{-# LANGUAGE TemplateHaskell #-}

module Sem.TypeInfo where

import Data.List
import Language.Haskell.GHC.ExactPrint.Parsers hiding (parseModuleFromString)
import Polysemy
import Polysemy.Input
import Sem.Ghcid
import Types
import Control.Lens
import MarkerUtils
import OccName hiding (tcName)

import Outputable


data TCInfo = TCInfo
  { tciName :: RdrName
  , tciVars :: [HsTyVarBndr GhcPs]
  , tciCons :: [ConDecl GhcPs]
  }


data TypeInfo m a where
  TypeInfo :: String -> TypeInfo m TCInfo

makeSem ''TypeInfo


runTypeInfo
    :: Members '[ Ghcid
                , Input DynFlags
                , Input LModule
                ] r
    => Sem (TypeInfo ': r) a
    -> Sem r a
runTypeInfo = interpret \case
  TypeInfo tycon -> do
    ast <- input
    case ast ^? decls . filtered ( (== Just tycon)
                                 . (^? tcName . to occNameString)) of
      Just x -> pure $ assembleTCI x
      Nothing -> do
        dflags <- input

        c <- doEval $ ":info " ++ tycon
        let d = ("data " ++)
              . takeUntilP (isPrefixOf "-- Defined")
              . dropUntil (isPrefixOf tycon)
              $ concat c
        -- pprTraceM "got:" $ text d

        (_, z)
          <- either (error . ("runTypeInfo: " ++) . snd) pure
           $ parseDecl dflags "<typeinfo>" d

        pure $ assembleTCI $ unLoc z


assembleTCI :: HsDecl GhcPs -> TCInfo
assembleTCI (TyClD _ (DataDecl _ (L _ tcname) (HsQTvs _ tyvars) _ (HsDataDefn _ _ _ _ _ cs _))) =
  TCInfo tcname (fmap unLoc tyvars) $ fmap unLoc cs
assembleTCI _ = error "assembleTCI"


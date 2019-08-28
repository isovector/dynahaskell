{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}

module Types
  ( module Types
  , module HsSyn
  , module HsTypes
  , Located
  , GenLocated (..)
  , noLoc
  , unLoc
  , DynFlags
  , OccName (..)
  , mkVarOcc
  , RdrName (..)
  , _Ctor'
  , position
  , position'
  , field
  , field'
  , module BasicTypes
  , Anns
  , Type
  ) where

import GHC.Generics
import Data.Data
import BasicTypes
import Data.Generics.Product.Positions
import Data.Generics.Sum.Constructors
import Data.Generics.Product.Fields
import GHC
import HsSyn
import HsTypes
import OccName
import RdrName
import Language.Haskell.GHC.ExactPrint

type Expr = HsExpr GhcPs
type Module = HsModule GhcPs

type LType = Located (HsType GhcPs)
type LExpr = Located (HsExpr GhcPs)
type LDecl = Located (HsDecl GhcPs)
type LModule = Located (HsModule GhcPs)


data Source = Source
  { sourceAnns :: Anns
  , sourceMod :: LModule
  }
  deriving (Data, Generic)

deriving instance Data Annotation


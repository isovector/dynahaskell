module Types
  ( module Types
  , module HsSyn
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
  , module BasicTypes
  ) where

import BasicTypes
import GHC
import HsSyn
import OccName
import RdrName
import Data.Generics.Product.Positions
import Data.Generics.Sum.Constructors

type Type = HsType GhcPs
type Expr = HsExpr GhcPs
type Module = HsModule GhcPs

type LType = Located (HsType GhcPs)
type LExpr = Located (HsExpr GhcPs)
type LModule = Located (HsModule GhcPs)


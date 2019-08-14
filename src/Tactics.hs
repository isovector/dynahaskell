{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tactics where

import Refinery.Tactic
import HsSyn
import GHC.Exts

type Type = HsType GhcPs

newtype Var = Var String
  deriving (Eq, Ord, Show, IsString)


data Judgement = Judgement [(Var, Type)] Type

type Tactic =


{-# LANGUAGE DeriveFunctor #-}

module Annotations where

import HsExpr
import GHC
import Unsafe.Coerce

data Annotation
  = NoAnnotation
  | Targeted
  | TypeError
  deriving (Eq, Ord, Show)

data Annotated a
  = Annotated [Annotation] a
  deriving (Eq, Ord, Show, Functor)


unAnnotated :: a -> Annotated a
unAnnotated = Annotated []

addAnnotation :: Annotation -> Annotated a -> Annotated a
addAnnotation a (Annotated as b) = Annotated (a : as) b


annotate :: Annotation -> LHsExpr GhcPs -> LHsExpr GhcPs
annotate ann = noLoc . HsPar (unsafeCoerce ann)


getAnnotation :: LHsExpr GhcPs -> Maybe Annotation
getAnnotation (L _ (HsPar ann _)) =
  case unsafeCoerce ann of
    NoAnnotation -> Nothing
    x -> Just x
getAnnotation _                   = Nothing


{-# LANGUAGE TupleSections #-}

module Printers where

import Generics.SYB hiding (Generic)
import Language.Haskell.GHC.ExactPrint
import Language.Haskell.GHC.ExactPrint.Annotater
import MarkerUtils
import Types



-- TODO(sandy): we should only do this when inserting a node
foo :: (Annotate a, Data a) => Anns -> Located a -> (Located a, Anns)
foo anns z = (z, addAnnotationsForPretty [] z anns)


hideMarkers :: Data a => a -> a
hideMarkers = everywhere (mkT hide)
  where
    hide (Underway _ _ z) = z
    hide z = z

deParen :: Expr -> Expr
deParen (HsPar _ (L _ a)) = deParen a
deParen a = a


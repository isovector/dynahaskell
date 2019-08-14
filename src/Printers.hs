{-# LANGUAGE TupleSections #-}

module Printers where

import GHC (AnnKeywordId (..))
import Generics.SYB hiding (Generic)
import HsSyn
import Language.Haskell.GHC.ExactPrint
import Language.Haskell.GHC.ExactPrint.Types (DeltaPos (..), KeywordId (..))
import RdrName
import SrcLoc
import MarkerUtils
import Language.Haskell.GHC.ExactPrint.Pretty
import Language.Haskell.GHC.ExactPrint.Annotater



-- TODO(sandy): we should only do this when inserting a node
foo :: (Annotate a, Data a) => Anns -> Located a -> (Located a, Anns)
foo anns z = (z, addAnnotationsForPretty [] z anns)


hideMarkers :: Data a => a -> a
hideMarkers = everywhere (mkT hide)
  where
    hide (Underway _ z) = z
    hide z = z

deParen :: HsExpr GhcPs -> HsExpr GhcPs
deParen (HsPar _ (L _ a)) = deParen a
deParen a = a


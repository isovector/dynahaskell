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


foo :: Data a => Anns -> a -> (a, Anns)
foo _anns z =
    let (a, (anns, _), _) = runTransform _anns
                          $ everywhereM (mkM mkSrc `extM` mkLSrc) z
     in (a, anns)
  where
    mkSrc :: Located RdrName -> Transform (Located RdrName)
    mkSrc (L loc z2) | loc == noSrcSpan = addAnnVal 0 z2
    mkSrc a = pure a

    mkLSrc :: LHsExpr GhcPs -> Transform (LHsExpr GhcPs)
    mkLSrc x@(L _ (Underway _ _)) = addAnns 1 [G AnnOpenP, G AnnCloseP] $ HsPar NoExt x
    mkLSrc (L loc x@(HsLit _ _)) | loc == noSrcSpan = addAnnVal 1 x
    mkLSrc (L loc x@(HsOverLit _ _)) | loc == noSrcSpan = addAnnVal 1 x
    mkLSrc (L loc x@(HsPar _ _)) | loc == noSrcSpan = addAnns 1 [G AnnOpenP, G AnnCloseP] x
    mkLSrc (L loc (HsUnboundVar _ (TrueExprHole occ))) | loc == noSrcSpan =
      (L loc . HsVar NoExt ) <$> addAnnVal 1 (Unqual occ)
    mkLSrc a = pure a


addAnns :: (Monad m, Data b) => Int -> [KeywordId] -> b -> TransformT m (Located b)
addAnns i anns z = do
  srcspan <- uniqueSrcSpanT
  let l' = L srcspan z
  addSimpleAnnT l' (DP (0, i)) $ fmap (, DP (0, 0)) anns
  pure l'


addAnnVal :: (Monad m, Data b) => Int -> b -> TransformT m (Located b)
addAnnVal i = addAnns i [G AnnVal]



hideMarkers :: Data a => a -> a
hideMarkers = everywhere (mkT hide)
  where
    hide (Underway _ z) = z
    hide z = z

deParen :: HsExpr GhcPs -> HsExpr GhcPs
deParen (HsPar _ (L _ a)) = deParen a
deParen a = a


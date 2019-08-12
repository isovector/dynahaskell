{-# LANGUAGE TupleSections #-}

module Printers where

import Data.Map ((\\))
import Data.Foldable
import GHC.Generics
import Generics.SYB hiding (Generic)
import HsSyn
import Language.Haskell.GHC.ExactPrint.Parsers hiding (parseModuleFromString)
import Language.Haskell.GHC.ExactPrint
import Language.Haskell.GHC.ExactPrint.Transform
import Language.Haskell.GHC.ExactPrint.Types (DeltaPos (..), KeywordId (..))
import GHC (SrcSpan, DynFlags, extensions, AnnKeywordId (..))
import MarkerUtils
import Markers
import OccName
import Outputable
import RdrName
import SrcLoc
import Control.Lens

foo :: Data a => Anns -> a -> (a, Anns)
foo _anns z = let (a, (anns, _), _) = runTransform _anns $ everywhereM (mkM mkSrc)
                                                         =<< everywhereM (mkM mkLSrc) z
               in (a, anns)
  where
    mkSrc :: Located RdrName -> Transform (Located RdrName)
    mkSrc (L loc z2) | loc == noSrcSpan = addAnnVal 0 z2
    mkSrc a = pure a

    mkLSrc :: LHsExpr GhcPs -> Transform (LHsExpr GhcPs)
    mkLSrc (L loc z@(HsLit _ _)) = addAnnVal 1 z
    mkLSrc (L loc z@(HsOverLit _ _)) = addAnnVal 1 z
    mkLSrc (L loc z@(HsPar _ _)) | loc == noSrcSpan = addAnns 1 [G AnnOpenP, G AnnCloseP] z
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


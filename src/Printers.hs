{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Printers where

import GHC hiding (getAnnotation)
import Polysemy
import Sem.Fresh
import Types
import Language.Haskell.GHC.ExactPrint
import Language.Haskell.GHC.ExactPrint.Print
import Language.Haskell.GHC.ExactPrint.Types
import Generics.SYB hiding (Generic, empty)
import Outputable
import Control.Lens
import FastString
import Annotations


ok :: (Data a, Member (Fresh Integer) r) => a -> Sem r a
ok = fmap sameVarSpans . everywhereM (mkM mkSrc)
  where
    mkSrc (UnhelpfulSpan _) = uniqueSpan
    mkSrc s                 = pure s


sameVarSpans :: Data a => a -> a
sameVarSpans = everywhere $ mkT sameSrc
  where
    sameSrc :: LExpr -> LExpr
    sameSrc (L _ (HsVar b q@(L c _))) = L c (HsVar b q)
    sameSrc z = z


uniqueSpan :: Member (Fresh Integer) r => Sem r SrcSpan
uniqueSpan = do
  col <- fromInteger <$> fresh
  let pos = mkSrcLoc (mkFastString "uniqueSpan") (-1) col
  return $ mkSrcSpan pos pos


prettySource :: Source -> String
prettySource (Source anns lmod) = exactPrint lmod anns

annotatedSource :: Source -> [Annotated String]
annotatedSource (Source anns lmod) = runIdentity $ exactPrintWithOptions annotatedPrintOpts lmod anns

pprToString :: DynFlags -> SDoc -> String
pprToString d = pprDebugAndThen d id empty


annotatedPrintOpts :: PrintOptions Identity [Annotated String]
annotatedPrintOpts =
  printOptions
    (\ast@(L src _) -> pure
                     . fmap (addAnnSpan src)
                     . fmap (maybe id addAnnotation $ getAnnotation =<< cast ast))
    (pure . pure . unAnnotated)
    (pure . pure . unAnnotated)
    NormalLayout


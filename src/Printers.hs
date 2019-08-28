{-# LANGUAGE TupleSections #-}

module Printers where

import  GHC hiding (getAnnotation)
import  Polysemy
import  Sem.Fresh
import  Types
import  Language.Haskell.GHC.ExactPrint
import  Language.Haskell.GHC.ExactPrint.Print
import  Language.Haskell.GHC.ExactPrint.Types
import  Generics.SYB hiding (Generic, empty)
import  Outputable
import  Control.Lens
import  FastString
import  Annotations


ok :: (Data a, Member (Fresh Integer) r) => a -> Sem r a
ok = everywhereM (mkM mkSrc)
  where
    mkSrc s | s == noSrcSpan = uniqueSpan
            | otherwise = pure s


uniqueSpan :: Member (Fresh Integer) r => Sem r SrcSpan
uniqueSpan = do
  col <- fromInteger <$> fresh
  let pos = mkSrcLoc (mkFastString "ghc-exactprint") (-1) col
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
    (\ast -> pure . fmap (maybe id addAnnotation $ getAnnotation =<< cast ast))
    (pure . pure . unAnnotated)
    (pure . pure . unAnnotated)
    NormalLayout


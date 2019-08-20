{-# LANGUAGE TupleSections #-}

module Printers where

import Generics.SYB hiding (Generic)
import Language.Haskell.GHC.ExactPrint
import GHC


ok :: Data a => a -> Transform a
ok = everywhereM (mkM mkSrc)
  where
    mkSrc :: SrcSpan -> Transform SrcSpan
    mkSrc s | s == noSrcSpan = uniqueSrcSpanT
            | otherwise = pure s


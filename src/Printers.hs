{-# LANGUAGE TupleSections #-}

module Printers where

import Polysemy
import Sem.Fresh
import Generics.SYB hiding (Generic)
import GHC
import FastString


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


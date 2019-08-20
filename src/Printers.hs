{-# LANGUAGE TupleSections #-}

module Printers where

import Generics.SYB hiding (Generic)
import Language.Haskell.GHC.ExactPrint
import GHC



-- -- TODO(sandy): we should only do this when inserting a node
-- foo :: (Annotate a, Data a) => Anns -> Located a -> (Located a, Anns)
-- foo anns z = (z, addAnnotationsForPretty [] z anns)



ok :: Data a => a -> Transform a
ok = everywhereM (mkM mkSrc)
  where
    mkSrc :: SrcSpan -> Transform SrcSpan
    mkSrc s | s == noSrcSpan = uniqueSrcSpanT
            | otherwise = pure s


-- foo :: Data a => Anns -> a -> (a, Anns)
--  foo _anns z = let (a, (anns, _), _) = runTransform _anns $ everywhereM (mkM mkSrc)
--                                                           =<< everywhereM (mkM mkLSrc) z
--                 in (a, anns)
--    where
--      mkSrc :: Located RdrName -> Transform (Located RdrName)
--      mkSrc (L loc z2) | loc == noSrcSpan = addAnnVal 0 z2
--      mkSrc a = pure a

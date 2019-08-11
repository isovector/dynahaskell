{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE ViewPatterns       #-}

module Lib where

import Data.Foldable
import GHC.Generics
import Generics.SYB hiding (Generic)
import HsSyn
import Language.Haskell.GHC.ExactPrint.Parsers
import MarkerUtils
import Markers
import OccName
import Outputable
import RdrName
import SrcLoc
import Control.Lens



unapply :: OccName -> HsExpr GhcPs -> HsExpr GhcPs
unapply occ (HsApp _ (L _ (HsVar _ (L _ (Unqual occ')))) (L _ z))
  | occ == occ' = z
unapply _ z = z

class Foo (a :: * -> *) where
  janky :: a x

instance Foo (M1 _1 _2 _3) where
  janky = todo


test :: Int
test = underway 1 (underway 0 (solve) + 5)

findInProgress :: [LHsDecl GhcPs] -> [LHsDecl GhcPs]
findInProgress = filter (everything (||) $ mkQ False $ matchOcc "underway")



main :: IO ()
main = do
  Right (_anns, (L _ a)) <- parseModule "src/Lib.hs"
  let inprog = findInProgress $ hsmodDecls a
  for_ inprog $ \p -> do
    pprTraceM "found" $ ppr p

    pprTraceM "replaced!" . ppr $ doSolve p


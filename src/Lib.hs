{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE PatternSynonyms  #-}
{-# LANGUAGE ViewPatterns     #-}

module Lib where

import Data.Foldable
import GHC.Generics
import Generics.SYB
import HsSyn
import Language.Haskell.GHC.ExactPrint.Parsers
import MarkerUtils
import Markers
import OccName
import Outputable
import RdrName
import SrcLoc
import Control.Lens
import Data.Data.Lens


unapply :: OccName -> HsExpr GhcPs -> HsExpr GhcPs
unapply occ (HsApp _ (L _ (HsVar _ (L _ (Unqual occ')))) (L _ z))
  | occ == occ' = z
unapply _ z = z

class Foo (a :: * -> *) where
  janky :: a x

instance Foo (M1 _1 _2 _3) where
  janky = todo


test :: Int
test = underway 1 (underway 0 13 + 5)

findInProgress :: [LHsDecl GhcPs] -> [LHsDecl GhcPs]
findInProgress = filter (everything (||) $ mkQ False $ f2 underwayOcc)



f2 :: OccName -> HsExpr GhcPs -> Bool
f2 occ (HsVar _ (L _ (Unqual occ'))) = occ == occ'
f2 _ _ = False








target :: Data a => a -> [HsExpr GhcPs]
target = everything (++) $ mkQ [] getUnderway

isUnderway :: HsExpr GhcPs -> Bool
isUnderway (Underway 0 _) = True
isUnderway _ = False

replaceUnderway :: HsExpr GhcPs -> HsExpr GhcPs -> HsExpr GhcPs
replaceUnderway z (Underway 0 _) = z
replaceUnderway _ z = z


main :: IO ()
main = do
  Right (_anns, (L _ a)) <- parseModule "src/Lib.hs"
  let inprog = findInProgress $ hsmodDecls a
  for_ inprog $ \p -> do
    pprTraceM "found" $ ppr p

    let unbound = HsUnboundVar NoExt (TrueExprHole $ mkVarOcc "_yo")

    pprTraceM "not replaced :(" . ppr $ p & biplate . filtered isUnderway .~ unbound
    pprTraceM "not replaced :(" . ppr $ p & upon target . _head .~ unbound
    pprTraceM "replaced!"       . ppr $ p & everywhere (mkT $ replaceUnderway unbound)


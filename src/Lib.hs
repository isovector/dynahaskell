{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE PatternSynonyms  #-}
{-# LANGUAGE ViewPatterns     #-}

module Lib where

import Data.Foldable
import Language.Haskell.GHC.ExactPrint.Parsers
import Outputable
import Markers
import GHC.Generics
import Generics.SYB
import HsSyn
import RdrName
import SrcLoc
import OccName
import Control.Lens
import Control.Lens.Plated
import Data.Data.Lens
import BasicTypes

unapply :: OccName -> HsExpr GhcPs -> HsExpr GhcPs
unapply occ (HsApp _ (L _ (HsVar _ (L _ (Unqual occ')))) (L _ z))
  | occ == occ' = z
unapply _ z = z

class Foo (a :: * -> *) where
  janky :: a x

instance Foo (M1 _1 _2 _3) where
  janky = inprogress

inprogressOcc :: OccName
inprogressOcc = mkVarOcc "inprogress"

underwayOcc :: OccName
underwayOcc = mkVarOcc "underway"


test :: Int
test = underway 1 (underway 0 (13) + 5)

findInProgress :: [LHsDecl GhcPs] -> [LHsDecl GhcPs]
findInProgress = filter (everything (||) $ mkQ False $ f2 underwayOcc)

pattern Underway :: Integer -> HsExpr GhcPs -> HsExpr GhcPs
pattern Underway i activity
  <- HsApp _ (L _ (HsApp _ (L _ ( HsVar _ (L _ (Unqual ((== underwayOcc) -> True)))))
                           (L _ (HsOverLit _ (OverLit _ (HsIntegral (IL _ False i)) _)))))
             (L _ activity)

unUnderway :: HsExpr GhcPs -> HsExpr GhcPs
unUnderway (Underway 0 z) = z
unUnderway a = a


f2 :: OccName -> HsExpr GhcPs -> Bool
f2 occ (HsVar _ (L _ (Unqual occ'))) = occ == occ'
f2 _ _ = False


main :: IO ()
main = do
  Right (anns, (L _ a)) <- parseModule "src/Lib.hs"
  let inprog = findInProgress $ hsmodDecls a
  for_ inprog $ \p -> do
    pprTraceM "found" $ ppr p
    pprTraceM "replaced" . ppr $ everywhere (mkT unUnderway) p
     -- p & biplate . filtered f2 .~ HsUnboundVar NoExt (TrueExprHole $ mkVarOcc "_yo")
      -- p & partsOf (biplate . filtered f2) . reversed . _head .~ HsUnboundVar NoExt (TrueExprHole $ mkVarOcc "_yo")
     -- p & biplate . filtered f2 .~ HsUnboundVar NoExt (TrueExprHole $ mkVarOcc "_yo")


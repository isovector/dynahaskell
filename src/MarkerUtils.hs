module MarkerUtils where

import BasicTypes
import Generics.SYB
import HsSyn
import OccName
import RdrName
import SrcLoc

pattern Underway :: Integer -> HsExpr GhcPs -> HsExpr GhcPs
pattern Underway i activity <-
  HsApp _ (L _ (HsApp _ (L _ ( HsVar _ (L _ (Unqual ((== underwayOcc) -> True)))))
                        (L _ (HsOverLit _ (OverLit _ (HsIntegral (IL _ False i)) _)))))
          (L _ activity) where
  Underway i activity =
    HsApp NoExt (noLoc (HsApp NoExt (noLoc ( HsVar NoExt (noLoc (Unqual underwayOcc))))
                (noLoc (HsOverLit NoExt (OverLit NoExt (HsIntegral (IL NoSourceText False i)) undefined)))))
          (noLoc activity) where

getUnderway :: HsExpr GhcPs -> [HsExpr GhcPs]
getUnderway (Underway 0 z) = [z]
getUnderway _ = []

todoOcc :: OccName
todoOcc = mkVarOcc "todo"

underwayOcc :: OccName
underwayOcc = mkVarOcc "underway"

unUnderway :: HsExpr GhcPs -> HsExpr GhcPs
unUnderway (Underway 0 z) = z
unUnderway a = a

succUnderway :: HsExpr GhcPs -> HsExpr GhcPs
succUnderway (Underway n z) = Underway (n + 1) z
succUnderway a = a

predUnderway :: HsExpr GhcPs -> HsExpr GhcPs
predUnderway (Underway n z) = Underway (n - 1) z
predUnderway a = a

finish :: Data a => a -> a
finish = everywhere (mkT predUnderway) . everywhere (mkT unUnderway)




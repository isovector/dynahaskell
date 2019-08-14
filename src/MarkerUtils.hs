{-# LANGUAGE RankNTypes           #-}

module MarkerUtils where

import Data.Maybe
import Data.Foldable
import BasicTypes
import Generics.SYB hiding (Generic)
import GHC.Generics
import HsSyn
import OccName
import RdrName
import SrcLoc
import Control.Lens
import Data.Generics.Product.Positions
import Data.Generics.Sum.Constructors
import Data.Data.Lens
import GenericOrphans ()
import MarkerLenses


pattern Underway :: Integer -> HsExpr GhcPs -> HsExpr GhcPs
pattern Underway i activity <-
  HsPar _ (L _ ((HsApp _ (L _ (HsApp _ (L _ ( HsVar _ (L _ (Unqual ((== underwayOcc) -> True)))))
                        (L _ (HsOverLit _ (OverLit _ (HsIntegral (IL _ False i)) _)))))
          (L _ (HsPar _ (L _ activity)))))) where
  Underway i activity =
    HsPar NoExt $ noLoc $
      HsApp NoExt (noLoc (HsApp NoExt (noLoc ( HsVar NoExt (noLoc (Unqual underwayOcc))))
                (noLoc (HsOverLit NoExt (OverLit NoExt (HsIntegral (IL (SourceText $ show i) False i))
                      $ HsLit NoExt (HsInt NoExt (IL (SourceText $ show i) False i)))))))
          (noLoc (HsPar NoExt (noLoc activity))) where

nowUnderway :: Data a => Traversal' a (HsExpr GhcPs)
nowUnderway = prevUnderway 0

prevUnderway :: Data a => Integer -> Traversal' a (HsExpr GhcPs)
prevUnderway n
  = prevUnderwayC n
  . _Ctor' @"HsPar"
  . position @2
  . loc
  . _Ctor' @"HsApp"
  . position @3
  . loc

nowUnderwayC :: Data a => Traversal' a (HsExpr GhcPs)
nowUnderwayC = prevUnderwayC 0

prevUnderwayC :: Data a => Integer -> Traversal' a (HsExpr GhcPs)
prevUnderwayC n = locate isUnderway
 where
   isUnderway (Underway n' _) = n == n'
   isUnderway _ = False

matchOcc :: String -> HsExpr GhcPs -> Bool
matchOcc occ (HsVar _ (L _ (Unqual occ'))) = mkVarOcc occ == occ'
matchOcc _ _ = False

locate :: (Data a, Data b) => (b -> Bool) -> Traversal' a b
locate f = biplate . deepOf uniplate (filtered f)

underwayOcc :: OccName
underwayOcc = mkVarOcc "underway"

nextSolve :: Data a => Traversal' a (HsExpr GhcPs)
nextSolve = locate (matchOcc "solve")

mkHole :: String -> HsExpr GhcPs
mkHole = HsVar NoExt . noLoc . Unqual . mkVarOcc

doSolve :: Data a => a -> a
doSolve p =
  case p ^? nextSolve of
    Just _ -> everywhere (mkT succUnderway) p & nextSolve .~ Underway 0 (mkHole "_to_solve")
    Nothing -> p



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
finish a = a & nowUnderwayC %~ unUnderway & everywhere (mkT predUnderway)

isFinished :: Data a => a -> Bool
isFinished a =
  isNothing $ asum
    [ a ^? nowUnderway . locate (matchOcc "todo")
    , a ^? nowUnderway . locate (matchOcc "solve")
    ]



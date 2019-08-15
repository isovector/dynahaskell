{-# LANGUAGE RankNTypes #-}

module MarkerUtils where

import Data.Maybe
import Data.Foldable
import Generics.SYB hiding (Generic)
import Control.Lens
import Data.Data.Lens
import GenericOrphans ()
import MarkerLenses
import Types


pattern Underway :: Integer -> Type -> Expr -> Expr
pattern Underway i ty activity <-
  HsPar _ (L _ ((HsApp _ (L _ (HsApp _ (L _ (

  HsAppType (HsWC _ (L _ ty)) (L _ (
  HsVar _ (L _ (Unqual ((== underwayOcc) -> True)))

                   ))))
                        (L _ (HsOverLit _ (OverLit _ (HsIntegral (IL _ False i)) _)))))
          (L _ (HsPar _ (L _ activity)))))) where
  Underway i ty activity =
    HsPar NoExt $ noLoc $
      HsApp NoExt (noLoc (HsApp NoExt (noLoc (

      HsAppType (HsWC NoExt $ noLoc ty) (noLoc $ HsVar NoExt (noLoc (Unqual underwayOcc)))))

                (noLoc (HsOverLit NoExt (OverLit NoExt (HsIntegral (IL (SourceText $ show i) False i))
                      $ HsLit NoExt (HsInt NoExt (IL (SourceText $ show i) False i)))))))
          (noLoc (HsPar NoExt (noLoc activity))) where

nowUnderway :: Data a => Traversal' a Expr
nowUnderway = prevUnderway 0

prevUnderway :: Data a => Integer -> Traversal' a Expr
prevUnderway n
  = prevUnderwayC n
  . _Ctor' @"HsPar"
  . position @2
  . loc
  . _Ctor' @"HsApp"
  . position @3
  . loc

underwayType :: Traversal' Expr Type
underwayType
  = _Ctor' @"HsPar"
  . position @2
  . loc
  . _Ctor' @"HsApp"
  . position @2
  . loc
  . _Ctor' @"HsApp"
  . position @2
  . loc
  . _Ctor' @"HsAppType"
  . position @1
  . _Ctor' @"HsWC"
  . position @2
  . loc

nowUnderwayC :: Data a => Traversal' a Expr
nowUnderwayC = prevUnderwayC 0

prevUnderwayC :: Data a => Integer -> Traversal' a Expr
prevUnderwayC n = locate isUnderway
 where
   isUnderway (Underway n' _ _) = n == n'
   isUnderway _ = False

matchOcc :: String -> Expr -> Bool
matchOcc occ (HsVar _ (L _ (Unqual occ'))) = mkVarOcc occ == occ'
matchOcc _ _ = False

locate :: (Data a, Data b) => (b -> Bool) -> Traversal' a b
locate f = biplate . deepOf uniplate (taking 1 $ filtered f)

underwayOcc :: OccName
underwayOcc = mkVarOcc "underway"

nextSolve :: Data a => Traversal' a Expr
nextSolve = locate (matchOcc "solve")

mkHole :: String -> Expr
mkHole = HsVar NoExt . noLoc . Unqual . mkVarOcc

doSolve :: Data a => Type -> a -> a
doSolve ty p =
  case p ^? nextSolve of
    Just _ -> everywhere (mkT succUnderway) p & nextSolve .~ Underway 0 ty (mkHole "_to_solve")
    Nothing -> p



unUnderway :: Expr -> Expr
unUnderway (Underway 0 _ z) = z
unUnderway a = a

succUnderway :: Expr -> Expr
succUnderway (Underway n t z) = Underway (n + 1) t z
succUnderway a = a

predUnderway :: Expr -> Expr
predUnderway (Underway n t z) = Underway (n - 1) t z
predUnderway a = a

finish :: Data a => a -> a
finish a = a & nowUnderwayC %~ unUnderway & everywhere (mkT predUnderway)

isFinished :: Data a => a -> Bool
isFinished a =
  isNothing $ asum
    [ a ^? nowUnderway . locate (matchOcc "todo")
    , a ^? nowUnderway . locate (matchOcc "solve")
    ]


decls :: Traversal' LModule (HsDecl GhcPs)
decls = loc
      . _Ctor' @"HsModule"
      . position @4
      . traverse
      . loc

tcName :: Traversal' (HsDecl GhcPs) OccName
tcName = _Ctor' @"TyClD"
       . position @2
       .  _Ctor' @"DataDecl"
       . position @2
       . loc
       . _Ctor' @"Unqual"


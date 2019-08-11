{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE KindSignatures   #-}

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

class Foo (a :: * -> *) where
  janky :: a x

instance Foo (M1 _1 _2 _3) where
  janky = inprogress

inprogressOcc :: OccName
inprogressOcc = mkVarOcc "inprogress"


findInProgress :: [LHsDecl GhcPs] -> [LHsDecl GhcPs]
findInProgress = filter (everything (||) $ mkQ False f2)
  where
    f2 :: HsExpr GhcPs -> Bool
    f2 (HsVar _ (L _ (Unqual occ))) = occ == inprogressOcc
    f2 _ = False


main :: IO ()
main = do
  Right (anns, (L _ a)) <- parseModule "src/Lib.hs"
  let inprog = findInProgress $ hsmodDecls a
  for_ inprog $ pprTraceM "hello" . ppr


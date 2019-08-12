{-# LANGUAGE TupleSections #-}

module Lib where

import GHC (SrcSpan, DynFlags)
import GHC.Generics
import Generics.SYB hiding (Generic)
import HsSyn
import Language.Haskell.GHC.ExactPrint
import Language.Haskell.GHC.ExactPrint.Parsers hiding (parseModuleFromString)
import MarkerUtils
import Markers
import Printers
import SrcLoc


parseModuleFromString
  :: FilePath
  -> String
  -> IO (Either (GHC.SrcSpan, String) (DynFlags, (Anns, Located (HsModule GhcPs))))
parseModuleFromString fp s = ghcWrapper $ do
  dflags <- initDynFlagsPure fp s
  return $ fmap (dflags, ) $ parseModuleFromStringInternal dflags fp s


class Foo (a :: * -> *) where
  janky :: a x

instance Foo (M1 _1 _2 _3) where
  janky = todo

findInProgress :: [LHsDecl GhcPs] -> [LHsDecl GhcPs]
findInProgress = filter (everything (||) $ mkQ False $ matchOcc "underway")



main :: IO ()
main = do
  contents <- readFile "src/Test.hs"
  Right (_dflags, (_anns, z)) <- parseModuleFromString "src/Lib.hs" contents

  let z' = doSolve z
      (z'', anns) = foo _anns z'

  putStrLn $ exactPrint z'' anns


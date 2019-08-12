{-# LANGUAGE TupleSections #-}

module Lib where

import Data.Foldable
import Data.Bifunctor
import Data.List
import Language.Haskell.Ghcid
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
import Outputable


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
  Right (dflags, (_anns, z)) <- parseModuleFromString "src/Lib.hs" contents

  let z' = doSolve z
      (z'', anns) = foo _anns z'

  writeFile "/tmp/dyna.hs" $ exactPrint z'' anns

  (g, _) <- startGhci "stack repl" (Just ".") (const $ const $ pure ())

  cts <- exec g ":l /tmp/dyna.hs"
  (_, t) <- either error pure . getHoleType dflags $ concat cts
  pprTraceM "type of hole" $ ppr t

  stopGhci g


getHoleType :: DynFlags -> String -> Either String (Anns, Located (HsType GhcPs))
getHoleType dflags
    = first snd
    . parseType dflags "<dyna>"
    . takeUntilP (\s -> isPrefixOf "Or perhaps" s || isPrefixOf "Where: " s)
    . drop (length preamble)
    . dropUntil (isPrefixOf preamble)
  where
    preamble = "Found hole: _to_solve :: "

dropUntil :: ([a] -> Bool) -> [a] -> [a]
dropUntil f s@(_:xs)
  | f s = s
  | otherwise = dropUntil f xs
dropUntil _ [] = []

takeUntilP :: ([a] -> Bool) -> [a] -> [a]
takeUntilP f s@(x:xs)
  | f s = []
  | otherwise = x : takeUntilP f xs
takeUntilP _ [] = []


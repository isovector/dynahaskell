{-# LANGUAGE TupleSections #-}

module Lib where

import Data.Foldable
import GHC.Generics
import Generics.SYB hiding (Generic)
import HsSyn
import Language.Haskell.GHC.ExactPrint.Parsers hiding (parseModuleFromString)
import Language.Haskell.GHC.ExactPrint
import GHC (SrcSpan, DynFlags, extensions)
import MarkerUtils
import Markers
import OccName
import Outputable
import RdrName
import SrcLoc
import Control.Lens


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


test :: Int
test = underway 1 (underway 0 (solve) + 5)

findInProgress :: [LHsDecl GhcPs] -> [LHsDecl GhcPs]
findInProgress = filter (everything (||) $ mkQ False $ matchOcc "underway")



main :: IO ()
main = do
  contents <- readFile "src/Lib.hs"
  Right (dflags, (_anns, z@(L _ a))) <- parseModuleFromString "src/Lib.hs" contents
  pprTraceM "flags" $ ppr $ extensions dflags

  let Right(_anns2, expr) = parseExpr dflags "src/Lib.hs" "solve"
  pprTraceM "parsed" $ ppr expr

  let z' = doSolve z
  pprTraceM "ok" $ ppr z'
  -- putStrLn $ exactPrint z' _anns


--   let inprog = findInProgress $ hsmodDecls a
--   for_ inprog $ \p -> do
--     pprTraceM "found" $ ppr p

--     pprTraceM "replaced!" . ppr $ doSolve p


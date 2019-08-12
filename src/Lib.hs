{-# LANGUAGE TupleSections #-}

module Lib where

import Data.Map ((\\))
import Data.Foldable
import GHC.Generics
import Generics.SYB hiding (Generic)
import HsSyn
import Language.Haskell.GHC.ExactPrint.Parsers hiding (parseModuleFromString)
import Language.Haskell.GHC.ExactPrint
import Language.Haskell.GHC.ExactPrint.Transform
import Language.Haskell.GHC.ExactPrint.Types (DeltaPos (..), KeywordId (..))
import GHC (SrcSpan, DynFlags, extensions, AnnKeywordId (..))
import MarkerUtils
import Markers
import OccName
import Outputable
import RdrName
import SrcLoc
import Control.Lens
import Printers


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
  let Right(_anns2, expr) = parseExpr dflags "src/Lib.hs" "solve"
  pprTraceM "parsed" $ ppr _anns

  let z' = doSolve z
      (z'', anns) = foo _anns z'

  putStrLn "\n\n\n-----------------------\n\n\n"
  pprTraceM "parsed2" $ ppr $ _anns

  putStrLn $ exactPrint z'' anns
  -- putStrLn "\n\n\n-----------------------\n\n\n"

--   pprTraceM "parsed" $ ppr anns


--   pprTraceM "" $ ppr z''


--   let inprog = findInProgress $ hsmodDecls a
--   for_ inprog $ \p -> do
--     pprTraceM "found" $ ppr p

--     pprTraceM "replaced!" . ppr $ doSolve p


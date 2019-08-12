{-# LANGUAGE TupleSections #-}

module Lib where

import GHC (SrcSpan, DynFlags)
import HsSyn
import Language.Haskell.GHC.ExactPrint
import Language.Haskell.GHC.ExactPrint.Parsers hiding (parseModuleFromString)
import MarkerUtils
import SrcLoc
import Outputable
import Sem.HoleType
import Sem.Ghcid
import Polysemy
import Polysemy.State
import Polysemy.Input
import Polysemy.Trace


parseModuleFromString
  :: FilePath
  -> String
  -> IO (Either (GHC.SrcSpan, String) (DynFlags, (Anns, Located (HsModule GhcPs))))
parseModuleFromString fp s = ghcWrapper $ do
  dflags <- initDynFlagsPure fp s
  return $ fmap (dflags, ) $ parseModuleFromStringInternal dflags fp s


main :: IO ()
main = do
  contents <- readFile "src/Test.hs"
  Right (dflags, (_anns, z)) <- parseModuleFromString "src/Lib.hs" contents


  t <- runM . traceToIO
            . runInputConst dflags
            . runInputConst _anns
            . evalState z
            . runGhcid
            . holeTypeToGhcid
            $ do
          start <- holeType
          modify $ doSolve
          end <- holeType
          pure (start, end)

  pprTraceM "type of hole" $ ppr t


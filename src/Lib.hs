{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TupleSections   #-}

module Lib where

import DynFlags
import GHC (SrcSpan, TypecheckedModule (..))
import HIE.Bios
import Language.Haskell.GHC.ExactPrint
import Language.Haskell.GHC.ExactPrint.Parsers hiding (parseModuleFromString)
import MarkerUtils
import Outputable hiding ((<+>), trace)
import Polysemy
import Polysemy.Input
import Polysemy.State
import Polysemy.Trace
import Sem.Anns
import Sem.Ghc
import Sem.HoleInfo
import Sem.View
import Sem.Fresh
import Types
import Tactics

import Data.Maybe


pprToString :: DynFlags -> SDoc -> String
pprToString d = pprDebugAndThen d id empty


parseModuleFromString
  :: FilePath
  -> String
  -> IO (Either (SrcSpan, String) (DynFlags, (Anns, LModule)))
parseModuleFromString fp s = ghcWrapper $ do
  dflags <- initDynFlagsPure fp s
  pure . fmap (dflags, )
       $ parseModuleFromStringInternal dflags fp s


main :: IO ()
main = do
  contents <- readFile "src/Test.hs"
  Right (dflags, (anns, lmod)) <- parseModuleFromString "src/Test.hs" contents

  runGHC
       . traceToIO
       . runInputConst dflags
       . stateAndInput anns
       . stateAndInput lmod
       . memoizeTypeStuff
       . runAnno
       . runHoleInfo
       . runFresh
       $ do
    hi <- holeInfo $ nextSolve
    expr <- tactic  (fst $ head hi) (deepen 10)
    spliceTree nextSolve (fromJust expr)

    pprTraceM "hi" . ppr =<< get @LModule



memoizeTypeStuff
    :: Members '[Input Anns, Embed Ghc, Embed IO, State LModule, Trace] r
    => Sem (View (Maybe TypecheckedModule) ': r) a
    -> Sem r a
memoizeTypeStuff = viewToState $ \lmod -> do
  anns <- input
  let printed = exactPrint lmod anns
  embed $ writeFile "/tmp/dyna.hs" printed
  fmap fst $ embed $ loadFile @Ghc ("/tmp/dyna.hs", "/tmp/dyna.hs")


stateAndInput :: s -> Sem (Input s ': State s ': r) a -> Sem r a
stateAndInput s = evalState s . runInputSem get


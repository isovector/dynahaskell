{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TupleSections   #-}

module Lib where

import Data.Bifunctor
import Data.Foldable
import Data.Maybe
import DynFlags
import GHC (SrcSpan)
import Language.Haskell.GHC.ExactPrint
import Language.Haskell.GHC.ExactPrint.Parsers hiding (parseModuleFromString)
import MarkerUtils
import Name
import Outputable hiding ((<+>), trace)
import Polysemy
import Polysemy.Input
import Polysemy.State
import Polysemy.Trace
import Sem.Anns
import Sem.Fresh
import Sem.Ghc
import Sem.HoleInfo
import Sem.Typecheck
import Tactics
import Types


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
  Right (dflags, uncurry Source -> src) <- parseModuleFromString "src/Test.hs" contents

  runGHC
       . traceToIO
       . runFresh @Integer
       . runInputConst dflags
       . runTypechecker
       . stateAndInput src
       . runAnno
       . runHoleInfo
       $ do
    holes <- holeInfo $ todo 0
    for_ holes $ \(goal, scope) -> do
      expr <- tactic goal (fmap (first nameOccName) scope) $ do
        destruct $ mkVarOcc "x"
        split
        assumption
      src' <- spliceTree (todo 0) (fromJust expr) src
      put src'

    Source anns' lmod' <- get
    trace $ exactPrint lmod' anns'


stateAndInput :: s -> Sem (Input s ': State s ': r) a -> Sem r a
stateAndInput s = evalState s . runInputSem get


{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TupleSections             #-}

module Lib where

import Brick.Main
import Control.Lens (taking)
import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import DynFlags
import GHC (SrcSpan)
import Language.Haskell.GHC.ExactPrint.Parsers hiding (parseModuleFromString)
import MarkerUtils
import Polysemy
import Polysemy.Input
import Polysemy.State
import Polysemy.Trace
import Sem.Anns
import Sem.Fresh
import Sem.Ghc
import Sem.HoleInfo
import Sem.Typecheck
import Types
import UI
import Zipper


parseFileModule
  :: FilePath
  -> IO (Either (SrcSpan, String) (DynFlags, Source))
parseFileModule fp = ghcWrapper $ do
  s <- liftIO $ readFile fp
  dflags <- initDynFlagsPure fp s
  pure . fmap ((dflags, ) . uncurry Source)
       $ parseModuleFromStringInternal dflags fp s


main :: IO ()
main = do
  let file = "src/Test.hs"
  (dflags, src) <- parseFileModule file >>= \case
    Right (dflags, src) -> pure (dflags, src)
    Left a -> error $ show a
  let dflags' = gopt_set dflags Opt_SuppressUniques

  runGHC file
       . traceToIO
       . runInputConst dflags'
       . runFresh @Integer
       . runTypechecker
       . stateAndInput (Zipper [] src [])
       . runAnno
       $ do
    -- doaction (anyUnderway . loc . _Ctor' @"OpApp" . _3 . loc . _Ctor' @"HsDo" . _3)
    -- z <- focus
    -- pprTraceM "yo" $ ppr $ z ^? anyUnderway . loc . _Ctor' @"OpApp" . _4 . loc
    -- trace $ prettySource z

    let l = taking 1 anyTodo
    holes <- holeInfo l src
    void $ defaultMain app $ defData l (listToMaybe holes)


stateAndInput :: s -> Sem (Input s ': State s ': r) a -> Sem r a
stateAndInput s = evalState s . runInputSem get


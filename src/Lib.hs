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
  s <- liftIO $ readFile "src/Test.hs"
  dflags <- initDynFlagsPure fp s
  pure . fmap ((dflags, ) . uncurry Source)
       $ parseModuleFromStringInternal dflags fp s


main :: IO ()
main = do
  Right (dflags, src) <- parseFileModule "src/Test.hs"

  runGHC
       . traceToIO
       . runInputConst dflags
       . runFresh @Integer
       . runInputConst dflags
       . runTypechecker
       . stateAndInput (Zipper [] src [])
       . runAnno
       $ do
    let l = taking 1 anyTodo
    holes <- holeInfo l src
    void $ defaultMain app $ defData l (listToMaybe holes)
--     for_ holes $ \(goal, scope) -> do
--       expr <- tactic goal (fmap (first nameOccName) scope) $ do
--         destruct $ mkVarOcc "x"
--         split
--         assumption
--       src' <- spliceTree (todo 0) (fromJust expr) src
--       put src'

--     Source anns' lmod' <- get
--     trace $ exactPrint lmod' anns'



stateAndInput :: s -> Sem (Input s ': State s ': r) a -> Sem r a
stateAndInput s = evalState s . runInputSem get


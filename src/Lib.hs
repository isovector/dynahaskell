{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TupleSections   #-}

module Lib where

import           Bag
import           Brick hiding (loc)
import qualified Brick.Main as M
import qualified Brick.Types as T
import           Brick.Widgets.Border
import           Brick.Widgets.Border.Style
import           Brick.Widgets.Edit
import           Control.Lens
import           Control.Monad
import           Data.Maybe
import           DynFlags
import           GHC (SrcSpan, TypecheckedModule (..))
import qualified Graphics.Vty as V
import           HIE.Bios
import           Id
import           Language.Haskell.GHC.ExactPrint
import           Language.Haskell.GHC.ExactPrint.Parsers hiding (parseModuleFromString)
import           MarkerLenses
import           MarkerUtils
import           Outputable hiding ((<+>))
import           Polysemy
import           Polysemy.IO
import           Polysemy.Input
import           Polysemy.State
import           Polysemy.Trace
import           Printers
import           Sem.Anns
import           Sem.Ghc
import           Sem.View
import           Tactics
import           TcRnTypes
import           Types
import           Var


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
  Right (dflags, (anns, lmod)) <- parseModuleFromString "src/Lib.hs" contents

  runGHC
       . traceToIO
       . runInputConst dflags
       . stateAndInput anns
       . stateAndInput lmod
       . memoizeTypeStuff
       . runAnno
       $ do
    (l, _) <- embed $ loadFile @Ghc ("src/Test.hs", "src/Test.hs")
    let Just l' = l
        binds = bagToList (tm_typechecked_source l')
          ^.. locate (matchVar "_hole") . _Ctor' @"HsVar" . position @1 . traverse
    pprTraceM "hi" $ vcat $ fmap (\(TcIdBndr b _) -> ppr $ (idName b, idType b) ) binds


memoizeTypeStuff
    :: Members '[Input Anns, Embed Ghc, Embed IO, State LModule] r
    => Sem (View (Maybe TypecheckedModule) ': r) a
    -> Sem r a
memoizeTypeStuff = viewToState $ \lmod -> do
  anns <- input
  let printed = exactPrint lmod anns
  embed $ writeFile "/tmp/dyna.hs" printed
  fmap fst $ embed $ loadFile @Ghc ("/tmp/dyna.hs", "/tmp/dyna.hs")


stateAndInput :: s -> Sem (Input s ': State s ': r) a -> Sem r a
stateAndInput s = evalState s . runInputSem get


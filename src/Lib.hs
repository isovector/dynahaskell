{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TupleSections   #-}

module Lib where

import TcRnTypes
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
import           GHC (SrcSpan, tm_typechecked_source)
import qualified Graphics.Vty as V
import           HIE.Bios
import           Language.Haskell.GHC.ExactPrint
import           Language.Haskell.GHC.ExactPrint.Parsers hiding (parseModuleFromString)
import           MarkerLenses
import           MarkerUtils
import           Outputable hiding ((<+>))
import           Polysemy
import           Polysemy.Input
import           Polysemy.State
import           Polysemy.Trace
import           Printers
import           Sem.Ghc
import           Tactics
import           Types
import           Var
import           Id
import DynFlags
import Polysemy.IO


pprToString :: DynFlags -> SDoc -> String
pprToString d = pprDebugAndThen d id empty


parseModuleFromString
  :: FilePath
  -> String
  -> IO (Either (SrcSpan, String) (DynFlags, Anns))
parseModuleFromString fp s = ghcWrapper $ do
  dflags <- initDynFlagsPure fp s
  pure . fmap ((dflags, ) . fst)
       $ parseModuleFromStringInternal dflags fp s


main :: IO ()
main = do
  contents <- readFile "src/Test.hs"
  Right (dflags, anns) <- parseModuleFromString "src/Lib.hs" contents

  runGHC
       . runEmbedPure (runTransform
       . traceToIO
       . runInputConst dflags
       $ do
    flags <- embed @Ghc $ getDynFlags
    (l, _) <- embed $ loadFile @Ghc ("src/Test.hs", "src/Test.hs")
    let Just l' = l
        binds = bagToList (tm_typechecked_source l')
          ^.. locate (matchVar "_hole") . _Ctor' @"HsVar" . position @1 . traverse
    pprTraceM "hi" $ vcat $ fmap (\(TcIdBndr b _) -> ppr $ (idName b, idType b) ) binds


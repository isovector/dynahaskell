{-# LANGUAGE TupleSections #-}

module Lib where

import Control.Monad
import           Brick
import qualified Brick.Main as M
import           GHC (SrcSpan, DynFlags)
import qualified Graphics.Vty as V
import           HsSyn
import           Language.Haskell.GHC.ExactPrint
import           Language.Haskell.GHC.ExactPrint.Parsers hiding (parseModuleFromString)
import           MarkerUtils
import           Outputable
import           Polysemy
import           Polysemy.Input
import           Polysemy.State
import           Polysemy.Trace
import           Sem.Ghcid
import           Sem.HoleType
import           SrcLoc
import qualified Brick.Types as T





pprToString :: DynFlags -> SDoc -> String
pprToString d = pprDebugAndThen d id empty


parseModuleFromString
  :: FilePath
  -> String
  -> IO (Either (GHC.SrcSpan, String) (DynFlags, (Anns, Located (HsModule GhcPs))))
parseModuleFromString fp s = ghcWrapper $ do
  dflags <- initDynFlagsPure fp s
  return $ fmap (dflags, ) $ parseModuleFromStringInternal dflags fp s


data Data = Data
  { dIsEditing :: Bool
  , dCurrentGoal :: String
  }

defData :: Data
defData = Data False ""



app
    :: Members '[Input DynFlags, HoleType, State (Located (HsModule GhcPs))] r
    => M.App Data e () (Sem r)
app = M.App
  { M.appDraw = \st -> [str $ dCurrentGoal st]
  , M.appStartEvent = pure
  , M.appHandleEvent = appEvent
  , M.appAttrMap = const $ attrMap V.defAttr []
  , M.appChooseCursor = M.neverShowCursor
  }

appEvent
    :: Members '[Input DynFlags, HoleType, State (Located (HsModule GhcPs))] r
    => Data
    -> T.BrickEvent () e
    -> T.EventM () (Sem r) (T.Next (Sem r) Data)
appEvent st (T.VtyEvent (V.EvKey (V.KChar 's') [])) = M.suspendAndResume $ do
  modify doSolve
  dflags <- input
  t <- holeType
  pure $ st
    { dCurrentGoal = pprToString dflags $ ppr t
    }
appEvent st (T.VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl])) = M.halt st
appEvent st (T.VtyEvent (V.EvKey (V.KChar 'q') [])) = M.halt st
appEvent st (T.VtyEvent (V.EvKey V.KEsc [])) = M.halt st
appEvent st _ = M.continue st


main :: IO ()
main = do
  contents <- readFile "src/Test.hs"
  Right (dflags, (anns, z)) <- parseModuleFromString "src/Lib.hs" contents


  runM . traceToIO
       . runInputConst dflags
       . runInputConst anns
       . evalState z
       . runGhcid
       . holeTypeToGhcid
       $ do
    void $ defaultMain app defData


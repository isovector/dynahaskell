{-# LANGUAGE TupleSections #-}

module Lib where

import           Brick
import qualified Brick.Main as M
import qualified Brick.Types as T
import           Brick.Widgets.Border
import           Brick.Widgets.Border.Style
import           Brick.Widgets.Edit
import           Control.Lens
import           Control.Monad
import           DynFlags (unsafeGlobalDynFlags)
import           GHC (SrcSpan, DynFlags)
import qualified Graphics.Vty as V
import           HsSyn
import           Language.Haskell.GHC.ExactPrint
import           Language.Haskell.GHC.ExactPrint.Parsers hiding (parseModuleFromString)
import           MarkerUtils
import           Outputable hiding ((<+>))
import           Polysemy
import           Polysemy.Input
import           Polysemy.State
import           Polysemy.Trace
import           Printers
import           Sem.Ghcid
import           Sem.HoleType
import           SrcLoc





pprToString :: DynFlags -> SDoc -> String
pprToString d = pprDebugAndThen d id empty


parseModuleFromString
  :: FilePath
  -> String
  -> IO (Either (GHC.SrcSpan, String) (DynFlags, (Anns, Located (HsModule GhcPs))))
parseModuleFromString fp s = ghcWrapper $ do
  dflags <- initDynFlagsPure fp s
  return $ fmap (dflags, ) $ parseModuleFromStringInternal dflags fp s


data Names = Editor
  deriving (Eq, Ord, Show)


data Data = Data
  { dIsEditing :: Bool
  , dCurrentGoal :: Maybe (HsType GhcPs)
  , dContext :: Maybe (HsExpr GhcPs)
  , dEditor :: Editor String Names
  , dFlags  :: DynFlags
  }


defData :: Data
defData = Data False Nothing Nothing (editor Editor (Just 1) "") unsafeGlobalDynFlags



drawUi :: Data -> [Widget Names]
drawUi st
  = pure
  . withBorderStyle unicode
  . borderWithLabel (str "The Glorious DynaHaskell Editor")
  $ vBox
    [ padAll 1 $ str $ "  _to_solve  ::" ++ maybe "error" (pprToString (dFlags st) . ppr) (dCurrentGoal st)
    , hBorder
    , padAll 1 $ str $ maybe "error" (pprToString (dFlags st) . ppr . hideMarkers) $ dContext st
    , hBorder
    ]


app
    :: Members '[Input DynFlags, HoleType, State (Located (HsModule GhcPs))] r
    => M.App Data e Names (Sem r)
app = M.App
  { M.appDraw = drawUi
  , M.appStartEvent = pure
  , M.appHandleEvent = appEvent
  , M.appAttrMap = const $ attrMap V.defAttr []
  , M.appChooseCursor = M.neverShowCursor
  }

appEvent
    :: Members '[Input DynFlags, HoleType, State (Located (HsModule GhcPs))] r
    => Data
    -> T.BrickEvent Names e
    -> T.EventM Names (Sem r) (T.Next (Sem r) Data)
appEvent st (T.VtyEvent (V.EvKey (V.KChar 's') [])) =
  M.performAction $ do
    modify doSolve
    res <- get
    t <- holeType
    pure $ st
      { dCurrentGoal = t
      , dContext = res ^? prevUnderway 1
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


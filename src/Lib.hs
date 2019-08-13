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
import           Sem.FillHole
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
defData = Data False Nothing Nothing resetEditor unsafeGlobalDynFlags

resetEditor :: Editor String Names
resetEditor = editor Editor (Just 1) ""


drawUi :: Data -> [Widget Names]
drawUi st
  = pure
  . withBorderStyle unicode
  . borderWithLabel (str "The Glorious DynaHaskell Editor")
  $ vBox
    [ padAll 1 $ str $ "  _to_solve  ::" ++ maybe "error" (pprToString (dFlags st) . ppr) (dCurrentGoal st)
    , hBorder
    , padAll 1 $ str $ maybe "error" (pprToString (dFlags st) . ppr . deParen . hideMarkers) $ dContext st
    , hBorder
    , padAll 1 $ renderEditor (str . concat) (dIsEditing st) (dEditor st)
    ]


app
    :: Members '[Input DynFlags, HoleType, State (Located (HsModule GhcPs)), FillHole] r
    => M.App Data e Names (Sem r)
app = M.App
  { M.appDraw = drawUi
  , M.appStartEvent = pure
  , M.appHandleEvent = appEvent
  , M.appAttrMap = const $ attrMap V.defAttr []
  , M.appChooseCursor = M.neverShowCursor
  }

appEvent
    :: Members '[Input DynFlags, HoleType, State (Located (HsModule GhcPs)), FillHole] r
    => Data
    -> T.BrickEvent Names e
    -> T.EventM Names (Sem r) (T.Next (Sem r) Data)
appEvent st (T.VtyEvent (V.EvKey (V.KEnter) [])) = do
  let c = getEditContents $ dEditor st
  M.performAction $ do
    fillHole (concat c) >>= \case
      FillOK -> do
        st' <- updateState st
        pure $ st'
          { dIsEditing = False
          , dEditor = resetEditor
          }
      BadParse ->
        pure $ st
          { dIsEditing = False
          , dEditor = resetEditor
          }
      BadType -> error "badtype"
appEvent st (T.VtyEvent e) | dIsEditing st = do
  edit' <- handleEditorEvent e (dEditor st)
  M.continue $ st
    { dEditor = edit'
    }
appEvent st (T.VtyEvent (V.EvKey (V.KChar 'e') [])) =
  M.continue $ st { dIsEditing = True }
appEvent st (T.VtyEvent (V.EvKey (V.KChar 's') [])) =
  M.performAction $ do
    modify doSolve
    updateState st
appEvent st (T.VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl])) = M.halt st
appEvent st (T.VtyEvent (V.EvKey (V.KChar 'q') [])) = M.halt st
appEvent st (T.VtyEvent (V.EvKey V.KEsc [])) = M.halt st
appEvent st _ = M.continue st


updateState :: Members '[HoleType, State (Located (HsModule GhcPs))] r => Data -> Sem r Data
updateState st = do
  res <- get
  t <- holeType
  pure $ st
    { dCurrentGoal = t
    , dContext = res ^? prevUnderway 1
    }



main :: IO ()
main = do
  contents <- readFile "src/Test.hs"
  Right (dflags, (anns, z)) <- parseModuleFromString "src/Lib.hs" contents


  runM . traceToIO
       . runInputConst dflags
       . runInputConst anns
       . evalState anns
       . evalState z
       . runGhcid
       . runFillHole
       . holeTypeToGhcid
       $ do
    void $ defaultMain app defData


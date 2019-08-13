{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TupleSections   #-}

module Lib where

import           Brick
import qualified Brick.Main as M
import qualified Brick.Types as T
import           Brick.Widgets.Border
import           Brick.Widgets.Border.Style
import           Brick.Widgets.Edit
import           Control.Lens
import           Control.Monad
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
  , dEditor :: Editor String Names
  }


defData :: Data
defData = Data False Nothing resetEditor

resetEditor :: Editor String Names
resetEditor = editor Editor (Just 1) ""


type Mems r =
  Members
    '[Input DynFlags
     , HoleType
     , State (Located (HsModule GhcPs))
     , FillHole
     , State Anns
     ] r


drawUi :: Mems r => Data -> Sem r [Widget Names]
drawUi st = do
  everything <- get @(Located (HsModule GhcPs))
  anns <- get
  dflags <- input

  pure . pure
       . withBorderStyle unicode
       . borderWithLabel (str "The Glorious DynaHaskell Editor")
       $ vBox
    [ padAll 1 $ str $ "  _to_solve  ::" ++ maybe "error" (pprToString dflags . ppr) (dCurrentGoal st)
    , hBorder
    , padAll 1 $ str $ maybe "" (pprToString dflags . ppr . deParen . hideMarkers) $ everything ^? prevUnderway 1
    , hBorder
    , padAll 1 . str . uncurry exactPrint $ foo anns everything
    , hBorder
    , padAll 1 $ renderEditor (str . concat) (dIsEditing st) (dEditor st)
    ]


app
    :: Mems r
    => M.App Data e Names (Sem r)
app = M.App
  { M.appDraw = drawUi
  , M.appStartEvent = pure
  , M.appHandleEvent = appEvent
  , M.appAttrMap = const $ attrMap V.defAttr []
  , M.appChooseCursor = M.neverShowCursor
  }

appEvent
    :: Mems r
    => Data
    -> T.BrickEvent Names e
    -> T.EventM Names (Sem r) (T.Next (Sem r) Data)
appEvent st (T.VtyEvent (V.EvKey (V.KEnter) [])) = do
  let c = getEditContents $ dEditor st
  M.performAction $ do
    fillHole (concat c) >>= \case
      FillOK -> do
        modify @(Located (HsModule GhcPs)) finish
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
appEvent st (T.VtyEvent (V.EvKey (V.KChar 'f') [])) =
  M.performAction $ do
    modify @(Located (HsModule GhcPs)) finish
    updateState st
appEvent st (T.VtyEvent (V.EvKey (V.KChar 's') [])) =
  M.performAction $ do
    modify @(Located (HsModule GhcPs)) doSolve
    updateState st
appEvent st (T.VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl])) = M.halt st
appEvent st (T.VtyEvent (V.EvKey (V.KChar 'q') [])) = M.halt st
appEvent st (T.VtyEvent (V.EvKey V.KEsc [])) = M.halt st
appEvent st _ = M.continue st


updateState :: Mems r => Data -> Sem r Data
updateState st = do
  t <- holeType
  pure $ st
    { dCurrentGoal = t
    }



main :: IO ()
main = do
  contents <- readFile "src/Test.hs"
  Right (dflags, (anns, z)) <- parseModuleFromString "src/Lib.hs" contents


  runM . traceToIO
       . runInputConst dflags
       . evalState anns
       . runInputSem @Anns get
       . evalState z
       . runGhcid
       . runFillHole
       . holeTypeToGhcid
       $ do
    void $ defaultMain app $ defData


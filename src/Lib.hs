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
import           Data.Maybe
import           GHC (SrcSpan)
import qualified Graphics.Vty as V
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
import           Sem.TypeInfo
import           Sem.Typecheck
import           Tactics
import           Types



pprToString :: DynFlags -> SDoc -> String
pprToString d = pprDebugAndThen d id empty


parseModuleFromString
  :: FilePath
  -> String
  -> IO (Either (SrcSpan, String) (DynFlags, (Anns, LModule)))
parseModuleFromString fp s = ghcWrapper $ do
  dflags <- initDynFlagsPure fp s
  return $ fmap (dflags, ) $ parseModuleFromStringInternal dflags fp s


data Names = Editor
  deriving (Eq, Ord, Show)


data Data = Data
  { dIsEditing :: Bool
  , dEditor :: Editor String Names
  }


defData :: Data
defData = Data False resetEditor

resetEditor :: Editor String Names
resetEditor = editor Editor (Just 1) ""


type Mems r =
  Members
    '[ FillHole
     , Input DynFlags
     , Input FreshInt
     , State Anns
     , State LModule
     , Trace
     , TypeInfo
     , Typecheck
     ] r


drawUi :: Mems r => Data -> Sem r [Widget Names]
drawUi st = do
  everything <- get @LModule
  anns <- get
  dflags <- input

  pure . pure
       . withBorderStyle unicode
       . borderWithLabel (str "The Glorious DynaHaskell Editor")
       $ vBox
    [ padAll 1 . str
               . mappend "  _to_solve  ::"
               . maybe "  error" (pprToString dflags . ppr)
               $ everything ^? nowUnderwayC . underwayType
    , hBorder
    , padAll 1 . str
               . maybe "" (pprToString dflags . ppr . deParen . hideMarkers)
               $ everything ^? prevUnderway 1
    , hBorder
    , padAll 1 . str
               . uncurry exactPrint
               . foo anns
               $ hideMarkers everything
    , hBorder
    , padAll 1 . renderEditor (str . concat) (dIsEditing st)
               $ dEditor st
    ]


app
    :: Mems r
    => M.App Data e Names (Sem r)
app = M.App
  { M.appDraw = drawUi
  , M.appStartEvent = pure
  , M.appHandleEvent = appEvent
  , M.appAttrMap = const $ attrMap V.defAttr []
  , M.appChooseCursor = const listToMaybe
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
        modify @LModule finish
      BadType -> error "badtype"
      BadParse -> pure ()
    pure $ st
      { dIsEditing = False
      , dEditor = resetEditor
      }
appEvent st (T.VtyEvent e) | dIsEditing st = do
  edit' <- handleEditorEvent e (dEditor st)
  M.continue $ st
    { dEditor = edit'
    }
appEvent st (T.VtyEvent (V.EvKey (V.KChar 't') [])) =
  M.performAction $ do
    ty <- typecheck nextSolve
    tactic ty (deepen 100) >>= \case
      Just expr -> modify @LModule $ nextSolve .~ expr
      Nothing -> pure ()
    pure st
appEvent st (T.VtyEvent (V.EvKey (V.KChar 'o') [])) =
  M.performAction $ do
    ty <- typecheck nextSolve
    tactic ty one >>= \case

      Just expr -> modify @LModule $ nextSolve .~ expr
      Nothing -> pure ()
    pure st
appEvent st (T.VtyEvent (V.EvKey (V.KChar 'f') [])) =
  M.performAction $ do
    modify @LModule finish
    pure st
appEvent st (T.VtyEvent (V.EvKey (V.KChar 's') [])) =
  M.performAction $ do
    runSolve
    pure $ st { dIsEditing = True }
appEvent st (T.VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl])) = M.halt st
appEvent st (T.VtyEvent (V.EvKey (V.KChar 'q') [])) = M.halt st
appEvent st (T.VtyEvent (V.EvKey V.KEsc [])) = M.halt st
appEvent st _ = M.continue st


runSolve :: Mems r => Sem r ()
runSolve = do
  gets @LModule (^? nextSolve) >>=
    \case
      Just _ -> do
        ty <- typecheck nextSolve
        modify @LModule $ doSolve ty
      Nothing -> pure ()



main :: IO ()
main = do
  contents <- readFile "src/Test.hs"
  Right (dflags, (anns, z)) <- parseModuleFromString "src/Lib.hs" contents


  runM . traceToIO
       . runInputConst dflags
       . evalState @Int 0
       . runInputSem (gets FreshInt <* modify @Int (+1))
       . evalState anns
       . runInputSem @Anns get
       . evalState z
       . runInputSem @LModule get
       . runGhcid
       . runTypeInfo
       . runFillHole
       . holeTypeToGhcid
       $ do
    void $ defaultMain app $ defData


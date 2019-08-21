{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TupleSections             #-}

module UI where

import           Brick hiding (loc)
import qualified Brick.Main as M
import qualified Brick.Types as T
import           Brick.Widgets.Border
import           Brick.Widgets.Border.Style
import           Brick.Widgets.Center
import           Brick.Widgets.Edit
import           Control.Lens hiding (holes)
import           Data.Bifunctor
import           Data.Data.Lens
import           Data.Foldable
import           Data.Maybe
import           Data.Traversable
import           GenericOrphans ()
import qualified Graphics.Vty as V
import           Language.Haskell.GHC.ExactPrint.Parsers (parseExpr, parseDecl)
import           Language.Haskell.GHC.ExactPrint.Transform (setPrecedingLines)
import           MarkerLenses
import           MarkerUtils
import           Name
import           Outputable (ppr)
import           Polysemy
import           Polysemy.Input
import           Polysemy.State
import           Printers
import           Refinery.Tactic ((<!>))
import           Sem.Anns
import           Sem.Fresh
import           Sem.Ghc
import           Sem.HoleInfo
import           Sem.Typecheck
import           Tactics
import           Types
import           Zipper


data Names = Editor
  deriving (Eq, Ord, Show)


data Data r = Data
  { dEditCont :: Maybe (String, String -> Data r -> Sem r (Data r))
  , dEditor :: Editor String Names
  , dTarget :: Traversal' LModule LExpr
  , dHoleInfo :: Maybe (Type, [(Name, Type)])
  }


defData :: Traversal' LModule LExpr -> Maybe (Type, [(Name, Type)]) -> Data r
defData = Data Nothing resetEditor

resetEditor :: Editor String Names
resetEditor = editor Editor (Just 1) ""


type Mems r =
  Members
    '[ Anno
     , Embed Ghc
     , Typecheck
     , Input DynFlags
     , State (Zipper Source)
     , Fresh Integer
     ] r


drawUi :: Mems r => Data r -> Sem r [Widget Names]
drawUi st = do
  dflags <- input
  src <- focus
  pure . pure
       . withBorderStyle unicode
       . borderWithLabel (str "The Glorious DynaHaskell Editor")
       $ vBox
    [ hBox
      [ padRight Max
                 . padAll 1 . str
                 $ prettySource src
      , vBorder
      , hLimit 40 $ vBox
        [ hCenter $ str $ pprToString dflags $ ppr $ fst <$> dHoleInfo st
        , hBorder
        , vBox $ do
            (_, bs) <- maybeToList $ dHoleInfo st
            (n, t) <- bs
            pure $ str $ mconcat
              [ occNameString $ nameOccName n
              , "  ::"
              , pprToString dflags $ ppr t
              ]
        ]
      ]
    , hBorder
    , padAll 1 $
        case dEditCont st of
          Just (prompt, _) ->
            hBox
            [ str $ prompt ++ "> "
            , renderEditor (str . concat) (isJust $ dEditCont st) $ dEditor st
            ]
          Nothing -> str ""
    ]


runTacticOf ::  Mems r => Tactic r -> Data r -> Sem r ()
runTacticOf t st = do
  let l :: Traversal' LModule LExpr
      l = dTarget st

  src <- focus
  holes <- holeInfo l src
  for_ holes $ \(goal, scope) -> do
    src' <- focus
    mexpr <- tactic goal (fmap (first nameOccName) scope) t
    for_ mexpr $ \expr -> do
      record =<< spliceTree (taking 1 l) expr src'


updateContext :: Mems r => Data r -> Sem r (Data r)
updateContext st = do
  src <- focus
  holes <- holeInfo (dTarget st) src
  pure $ st { dHoleInfo = listToMaybe holes }


app :: Mems r => M.App (Data r) e Names (Sem r)
app = M.App
  { M.appDraw         = drawUi
  , M.appStartEvent   = pure
  , M.appHandleEvent  = appEvent
  , M.appAttrMap      = const $ attrMap V.defAttr []
  , M.appChooseCursor = const listToMaybe
  }

appEvent
    :: Mems r
    => Data r
    -> T.BrickEvent Names e
    -> T.EventM Names (Sem r) (T.Next (Sem r) (Data r))
appEvent st (T.VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl])) = M.halt st
appEvent st (T.VtyEvent e) | Just (_, cont) <- dEditCont st = do
  case e of
    V.EvKey V.KEsc [] ->
      M.continue $ st
        { dEditCont = Nothing
        , dEditor = resetEditor
        }
    V.EvKey V.KEnter [] -> do
      let c = getEditContents $ dEditor st
      M.performAction $ do
        cont (concat c) $ st
          { dEditCont = Nothing
          , dEditor = resetEditor
          }
    _ -> do
      edit' <- handleEditorEvent e (dEditor st)
      M.continue $ st
        { dEditor = edit'
        }
appEvent st (T.VtyEvent (V.EvKey (V.KChar 'e') [])) =
  M.performAction $ do
    withEdit st "Edit" $ \c st' -> do
      parseLExpr c >>= \case
        Just (anns', lexpr) -> do
          t <- focus
          Source anns t' <- spliceTree (dTarget st) lexpr t
          record $ Source (anns <> anns') t'
          updateContext st'
        Nothing -> pure st'
appEvent st (T.VtyEvent (V.EvKey (V.KChar 's') [])) =
  M.performAction $ do
    withEdit st "Destruct Term" $ \c st' -> do
      runTacticOf (destruct $ mkVarOcc c) st
      updateContext st'
appEvent st (T.VtyEvent (V.EvKey (V.KChar 'i') [])) =
  M.performAction $ do
    withEdit st "Introduce Name" $ \nm st' -> do
      withEdit st' "Introduce Type" $ \ty st'' -> do
        decstr <- buildLDecl nm ty
        parseLDecl decstr >>= \case
          Just (anns', ldecl) -> do
            Source anns t <- focus
            record $ Source (anns <> anns') $ t & loc . biplate <>~ [ldecl]
            updateContext st''
          Nothing -> pure st''


appEvent st (T.VtyEvent (V.EvKey (V.KChar 'a') [])) =
  M.performAction $ do
    runTacticOf auto st
    updateContext st
appEvent st (T.VtyEvent (V.EvKey (V.KChar 't') [])) =
  M.performAction $ do
    runTacticOf (intro "x" <!> assumption <!> split) st
    updateContext st
appEvent st (T.VtyEvent (V.EvKey V.KLeft [])) =
  M.performAction $ do
    undo
    updateContext st
appEvent st (T.VtyEvent (V.EvKey V.KRight [])) =
  M.performAction $ do
    redo
    updateContext st
appEvent st (T.VtyEvent (V.EvKey (V.KChar 'q') [])) = M.halt st
appEvent st (T.VtyEvent (V.EvKey V.KEsc [])) = M.halt st
appEvent st _ = M.continue st


withEdit
    :: Data r
    -> String
    -> (String
        -> Data r
        -> Sem r (Data r))
    -> Sem r (Data r)
withEdit st prompt cont = pure $ st
  { dEditCont = Just (prompt, cont)
  , dEditor = resetEditor
  }


buildLDecl :: Mems r => String -> String -> Sem r String
buildLDecl nm ty = do
  dflags <- input
  td <- newTodo
  pure $ mconcat
    [ nm , " :: " , ty , "\n"
    , nm , " = " , pprToString dflags $ ppr td
    ]


parseLExpr :: Mems r => String -> Sem r (Maybe (Anns, LExpr))
parseLExpr s = do
  dflags <- input
  n <- fresh
  pure $ hush $ parseExpr dflags ("parseLExpr:" ++ show n) s


parseLDecl :: Mems r => String -> Sem r (Maybe (Anns, LDecl))
parseLDecl s = do
  dflags <- input
  n <- fresh
  let mz = hush $ parseDecl dflags ("parseLDecl:" ++ show n) s
  for mz $ \(anns, decl) -> do
    let anns' = setPrecedingLines decl 2 0 anns
    pure (anns', decl)


hush :: Either b a -> Maybe a
hush = either (const Nothing) Just


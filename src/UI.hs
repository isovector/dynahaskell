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
import           Data.Foldable
import           Data.Maybe
import qualified Graphics.Vty as V
import           Name
import           Outputable hiding ((<+>))
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


data Names = Editor
  deriving (Eq, Ord, Show)


data Data = Data
  { dIsEditing :: Bool
  , dEditor :: Editor String Names
  , dTarget :: Traversal' LModule LExpr
  , dHoleInfo :: Maybe (Type, [(Name, Type)])
  }


defData :: Traversal' LModule LExpr -> Maybe (Type, [(Name, Type)]) -> Data
defData = Data False resetEditor

resetEditor :: Editor String Names
resetEditor = editor Editor (Just 1) ""


type Mems r =
  Members
    '[ Anno
     , Embed Ghc
     , Typecheck
     , Input DynFlags
     , State Source
     , Fresh Integer
     ] r


drawUi :: Mems r => Data -> Sem r [Widget Names]
drawUi st = do
  dflags <- input
  src <- get
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
    , padAll 1 . renderEditor (str . concat) (dIsEditing st)
               $ dEditor st
    ]


runTacticOf ::  Mems r => Tactic r -> Data -> Sem r ()
runTacticOf t st = do
  let l :: Traversal' LModule LExpr
      l = dTarget st

  src <- get
  holes <- holeInfo l src
  for_ holes $ \(goal, scope) -> do
    src' <- get
    mexpr <- tactic goal (fmap (first nameOccName) scope) t
    for_ mexpr $ \expr -> do
      put =<< spliceTree (taking 1 l) expr src'


updateContext :: Mems r => Data -> Sem r Data
updateContext st = do
  src <- get
  holes <- holeInfo (dTarget st) src
  pure $
    st
      { dHoleInfo = listToMaybe holes
      }




app
    :: Mems r
    => M.App Data e Names (Sem r)
app = M.App
  { M.appDraw         = drawUi
  , M.appStartEvent   = pure
  , M.appHandleEvent  = appEvent
  , M.appAttrMap      = const $ attrMap V.defAttr []
  , M.appChooseCursor = const listToMaybe
  }

appEvent
    :: Mems r
    => Data
    -> T.BrickEvent Names e
    -> T.EventM Names (Sem r) (T.Next (Sem r) Data)
-- -- appEvent st (T.VtyEvent (V.EvKey (V.KEnter) [])) = do
-- --   let c = getEditContents $ dEditor st
-- --   M.performAction $ do
-- --     fillHole (concat c) >>= \case
-- --       FillOK -> do
-- --         modify @LModule finish
-- --       BadType -> error "badtype"
-- --       BadParse -> pure ()
-- --     pure $ st
-- --       { dIsEditing = False
-- --       , dEditor = resetEditor
-- --       }
-- appEvent st (T.VtyEvent e) | dIsEditing st = do
--   edit' <- handleEditorEvent e (dEditor st)
--   M.continue $ st
--     { dEditor = edit'
--     }
-- appEvent st (T.VtyEvent (V.EvKey (V.KChar 't') [])) =
--   M.performAction $ do
--     ty <- typecheck nextSolve
--     tactic ty (deepen 100) >>= \case
--       Just expr -> modify @LModule $ nextSolve .~ expr
--       Nothing -> pure ()
--     pure st
appEvent st (T.VtyEvent (V.EvKey (V.KChar 't') [])) =
  M.performAction $ do
    runTacticOf (intro "x" <!> assumption <!> split) st
    updateContext st
appEvent st (T.VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl])) = M.halt st
appEvent st (T.VtyEvent (V.EvKey (V.KChar 'q') [])) = M.halt st
appEvent st (T.VtyEvent (V.EvKey V.KEsc [])) = M.halt st
appEvent st _ = M.continue st



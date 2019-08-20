{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TupleSections   #-}

module UI where

import           Bag
import           Brick hiding (loc)
import qualified Brick.Main as M
import qualified Brick.Types as T
import           Brick.Widgets.Border
import           Brick.Widgets.Border.Style
import           Brick.Widgets.Edit
import           Brick.Widgets.Center
import           Control.Lens
import           Control.Monad
import           Data.Maybe
import           GHC (SrcSpan, TypecheckedModule (..))
import qualified Graphics.Vty as V
import           HIE.Bios
import           Id
import           Language.Haskell.GHC.ExactPrint
import           Language.Haskell.GHC.ExactPrint.Parsers hiding (parseModuleFromString)
import           MarkerLenses
import           MarkerUtils
import           Name
import           Outputable hiding ((<+>))
import           Polysemy
import           Polysemy.Input
import           Polysemy.State
import           Polysemy.Trace
import           Printers
import           Sem.Anns
import           Sem.Ghc
import           Sem.HoleInfo
import           Sem.Typecheck
import           Sem.View
import           Tactics
import           TcRnTypes
import           Types
import           Var


data Names = Editor
  deriving (Eq, Ord, Show)


data Data = Data
  { dIsEditing :: Bool
  , dEditor :: Editor String Names
  , dSource :: Source
  , dTarget :: Traversal' LModule LExpr
  , dHoleInfo :: Maybe (Type, [(Name, Type)])
  }


defData :: Traversal' LModule LExpr -> Maybe (Type, [(Name, Type)]) -> Source -> Data
defData l info src = Data False resetEditor src l info

resetEditor :: Editor String Names
resetEditor = editor Editor (Just 1) ""


type Mems r =
  Members
    '[ Anno
     , Embed Ghc
     , Typecheck
     , Input DynFlags
     ] r


drawUi :: Mems r => Data -> Sem r [Widget Names]
drawUi st = do
  dflags <- input
  pure . pure
       . withBorderStyle unicode
       . borderWithLabel (str "The Glorious DynaHaskell Editor")
       $ vBox
    [ hBox
      [ padAll 1 . str
                 . prettySource
                 $ dSource st
      , vBorder
      , vBox
        [ center $ str $ pprToString dflags $ ppr $ fst <$> dHoleInfo st
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
-- appEvent st (T.VtyEvent (V.EvKey (V.KChar 'o') [])) =
--   M.performAction $ do
--     ty <- typecheck nextSolve
--     tactic ty one >>= \case

--       Just expr -> modify @LModule $ nextSolve .~ expr
--       Nothing -> pure ()
--     pure st
appEvent st (T.VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl])) = M.halt st
appEvent st (T.VtyEvent (V.EvKey (V.KChar 'q') [])) = M.halt st
appEvent st (T.VtyEvent (V.EvKey V.KEsc [])) = M.halt st
appEvent st _ = M.continue st



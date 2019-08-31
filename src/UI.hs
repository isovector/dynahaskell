{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TupleSections             #-}

module UI where

import           Annotations
import           Brick hiding (loc)
import qualified Brick.Main as M
import qualified Brick.Types as T
import           Brick.Widgets.Border
import           Brick.Widgets.Border.Style
import           Brick.Widgets.Center
import           Brick.Widgets.Edit
import           Control.Arrow ((***))
import           Control.Lens hiding (holes)
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Markup as MU
import           EditorActions
import           GHC
import qualified Graphics.Vty as V
import           Graphics.Vty hiding (Input, text)
import           MarkerUtils
import           Name
import           Outputable (ppr, text)
import           Polysemy
import           Polysemy.Input
import           Printers
import           Tactics
import qualified Trie as Trie
import           Types
import           UI.Stuff
import           Zipper


vim
    :: Mems r => Trie.Trie V.Key
             (Last ( Data r
                  -> T.EventM Names (Sem r) (T.Next (Sem r) (Data r))
                   ))
vim = Trie.fromList $ fmap (mapChars *** Last . Just)
  -- Navigation
  [ "gg" --> continuing $ vScrollToBeginning scroller
  , "G"  --> continuing $ vScrollToEnd scroller
  , "j"  --> continuing $ vScrollBy scroller 1
  , "k"  --> continuing $ vScrollBy scroller (-1)

  -- Selection
  , "st" --> selecting $ taking 1 anyTodo

  -- Undo/redo
  , "←"  --> invalidating (<$ undo)
  , "→"  --> invalidating (<$ redo)

  -- Editing/tactics
  , "a"  --> invalidating $ tactful auto
  , "t"  --> invalidating $ tactful one
  , "h"  --> sem . prompt "Homo" $ \d ->
                 tactfulInvalid $ homo (mkVarOcc d) >> auto
  , "d"  --> sem . prompt "Destruct"
                 $ tactfulInvalid . destruct . mkVarOcc
  , "e"  --> sem . prompt "Edit" $ \c st -> flip invalidateSuccess st
                 $ edit (dTarget st) c
  , "i"  --> sem . prompt "Intro Name" $ \nm ->
                   prompt "Intro Type" $ \ty ->
               invalidateSuccess $ introduceTopLevel nm ty

  -- Monadic stuff
  , "moa"  --> invalidating $ tactful dobodyblock
  , "mob"  --> sem . prompt "Bind Name"
                 $ tactfulInvalid . dobindblock
  , "ma"   --> invalidating $ purely $ doaction $ anyUnderway . underwayStmts
  , "mb"   --> sem . prompt "Bind Name" $ \nm st ->
                 flip invalidateSuccess st $
                   Just () <$ (dobind nm $ anyUnderway . underwayStmts)

  -- Quit
  , "q" --> M.halt
  ]


drawUi :: Mems r => Data r -> Sem r [Widget Names]
drawUi st = do
  dflags <- input
  src <- focus
  pure . pure
       . withBorderStyle unicode
       . borderWithLabel (str "The Glorious DynaHaskell Editor")
       $ vBox
    [ hBox
      [ viewport Code Vertical
          -- . cached CodeCache
          . padRight Max
          . padAll 1
          . markup
          . buildMarkup
          . annotatedSource
          $ src & position @2 . dTarget st %~ annotate Targeted
      , vBorder
      , hLimit 70 $ vBox
        [ hCenter $ str $ pprToString dflags $ maybe (text "") (ppr . fst) $ dHoleInfo st
        , hBorder
        , vBox $ do
            (_, bs) <- maybeToList $ dHoleInfo st
            (n, t) <- bs
            pure $ strWrap $ mconcat
              [ occNameString $ nameOccName n
              , "  ::" , pprToString dflags $ ppr t
              ]
        ]
      ]
    , hBorder
    , padAll 1 $
        flip (maybe $ str "") (dEditCont st) $ \(p, _) ->
          hBox [ str $ p ++ "> "
               , renderEditor (str . concat) (isJust $ dEditCont st) $ dEditor st
               ]
    ]


markup :: MU.Markup ([Annotation], [SrcSpan]) -> Widget Names
markup m =
  let mlines = MU.markupToList m
   in vBox $ do
        line <- mlines
        pure $ hBox $
          case line of
            [] -> pure $ str " "
            _ -> do
             (t, (anns, srcs)) <- line
             pure . withAttr (foldMap (attrName . show) anns)
                  . clickable (Clickable srcs)
                  $ txt t


buildMarkup :: [Annotated String] -> MU.Markup ([Annotation], [SrcSpan])
buildMarkup =
  MU.fromList
  . fmap (\(Annotated as src a) -> (T.pack a, (as, src)))


app :: Mems r => M.App (Data r) e Names (Sem r)
app = M.App
  { M.appDraw         = drawUi
  , M.appStartEvent   = \st -> do
      vty <- Brick.getVtyHandle
      let output = outputIface vty
      when (supportsMode output Mouse) $
        liftIO $ setMode output Mouse True
      pure st
  , M.appHandleEvent  = appEvent
  , M.appAttrMap      = const $ attrMap V.defAttr $
      [ (attrName $ show Targeted,  defAttr `withForeColor` blue)
      , (attrName $ show TypeError, defAttr `withForeColor` red)
      ]
  , M.appChooseCursor = const listToMaybe
  }


defData
    :: Mems r
    => Traversal' LModule LExpr
    -> Maybe (Type, [(Name, Type)])
    -> Data r
defData = Data Nothing resetEditor (Trie.Vim vim vim)


appEvent
    :: Mems r
    => Data r
    -> T.BrickEvent Names e
    -> T.EventM Names (Sem r) (T.Next (Sem r) (Data r))
appEvent st (T.MouseDown (Clickable srcs) _ _ _) = do
  M.performAction $ updateContext st
    { dTarget = locate $ \(L src _) -> last srcs == src
    }
appEvent st (T.VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl])) = M.halt st
appEvent st (T.VtyEvent e) | Just (_, cont) <- dEditCont st = do
  case e of
    V.EvKey V.KEsc [] ->
      M.continue $ st
        { dEditCont = Nothing
        , dEditor = resetEditor
        }
    V.EvKey V.KEnter [] -> do
      invalidateCacheEntry CodeCache
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
appEvent st (T.VtyEvent (V.EvKey k [])) = do
  case Trie.pump (dVimDFA st) k of
    (Just a, v)  -> a $ st { dVimDFA = v }
    (Nothing, v) -> continue $ st { dVimDFA = v }
appEvent st _ = M.continue st


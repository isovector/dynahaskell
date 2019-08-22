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
import           Control.Arrow ((***))
import           Control.Lens hiding (holes)
import           Data.Data.Lens
import           Data.Maybe
import           Data.Monoid
import           Data.Traversable
import           GenericOrphans ()
import           Generics.SYB (everywhereM, mkM)
import qualified Graphics.Vty as V
import           Language.Haskell.GHC.ExactPrint.Parsers (parseExpr, parseDecl)
import           Language.Haskell.GHC.ExactPrint.Transform (setPrecedingLines)
import           MarkerLenses
import           MarkerUtils
import           Name
import           Outputable (ppr)
import           Polysemy
import           Polysemy.Input
import           Printers
import           Sem.Anns
import           Sem.Fresh
import           Tactics
import qualified Trie as T
import           Types
import           UI.Stuff
import           UniPatterns
import           Zipper


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
          . cached CodeCache
          . padRight Max
          . padAll 1 . str
          $ prettySource src
      , vBorder
      , hLimit 70 $ vBox
        [ hCenter $ str $ pprToString dflags $ ppr $ fst <$> dHoleInfo st
        , hBorder
        , vBox $ do
            (_, bs) <- maybeToList $ dHoleInfo st
            (n, t) <- bs
            pure $ strWrap $ mconcat
              [ occNameString $ nameOccName n
              , "  ::"
              , pprToString dflags $ ppr t
              ]
        ]
      ]
    , hBorder
    , padAll 1 $
        case dEditCont st of
          Just (p, _) ->
            hBox
            [ str $ p ++ "> "
            , renderEditor (str . concat) (isJust $ dEditCont st) $ dEditor st
            ]
          Nothing -> str ""
    ]


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
appEvent st _ = M.continue st


vim
    :: Mems r => T.Trie V.Key
             (Last ( Data r
                  -> T.EventM Names (Sem r) (T.Next (Sem r) (Data r))
                   ))
vim = T.fromList $ fmap (mapChars *** Last . Just)
  [ "gg" --> continuing $ vScrollToBeginning scroller
  , "G"  --> continuing $ vScrollToEnd scroller
  , "↑"  --> continuing $ vScrollBy scroller (-1)
  , "↓"  --> continuing $ vScrollBy scroller 1
  , "←"  --> invalidating (<$ undo)
  , "→"  --> invalidating (<$ redo)
  , "a"  --> invalidating $ tactful auto
  , "t"  --> invalidating $ tactful one
  , "d"  --> sem $ prompt "Destruct" $ tactful . destruct . mkVarOcc
  , "e"  -->
      sem $ prompt "Edit" $ \c st -> do
        parseLExpr c >>= orMatch (pure st) \(Just (anns', lexpr)) -> do
          t <- focus
          Source anns t' <- spliceTree (dTarget st) lexpr t
          record $ Source (anns <> anns') t'
          updateContext st
  , "i"  -->
      sem $ prompt "Intro Name" $ \nm -> prompt "Intro Type" $ \ty st' -> do
        decstr <- buildLDecl nm ty
        parseLDecl decstr >>= orMatch (pure st') \(Just (anns', ldecl)) -> do
          Source anns t <- focus
          record $ Source (anns <> anns') $ t & loc . biplate <>~ [ldecl]
          updateContext st'
  , "q" --> M.halt
  ]


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
  for (hush $ parseExpr dflags ("parseLExpr:" ++ show n) s) $ \(anns, lexpr) -> do
    lexpr' <-
      everywhereM
        ( mkM \case
            L _ (EWildPat _) -> parenthesizeHsExpr appPrec <$> newTodo
            a -> pure a
        ) lexpr
    pure (anns, lexpr')


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


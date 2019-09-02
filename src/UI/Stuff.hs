{-# LANGUAGE ConstraintKinds #-}

module UI.Stuff where

import           Brick hiding (loc)
import qualified Brick.Main as M
import qualified Brick.Types as T
import           Brick.Widgets.Edit
import           Control.Lens hiding (holes)
import           Data.Bifunctor
import           Data.Foldable
import           Data.Maybe
import           GHC (SrcSpan)
import           GenericOrphans ()
import qualified Graphics.Vty as V
import           Name
import           Polysemy
import           Polysemy.Input
import           Polysemy.State
import           Sem.Anns
import           Sem.FileProvider
import           Sem.Fresh
import           Sem.Ghc
import           Sem.HoleInfo
import           Sem.Typecheck
import           Tactics
import           TacticsV2
import qualified Trie as T
import           Types
import           Zipper


data Names = Editor | Code | CodeCache | Clickable [SrcSpan]
  deriving (Eq, Ord, Show)

type Action r = Data r -> T.EventM Names (Sem r) (T.Next (Sem r) (Data r))
type Vim r = T.Vim V.Key (Action r)


data Data r = Data
  { dEditCont :: Maybe (String, String -> Data r -> Sem r (Data r))
  , dEditor   :: Editor String Names
  , dVimDFA   :: Vim r
  , dTarget   :: Traversal' LModule LExpr
  , dHoleInfo :: Maybe (Type, [(Name, Type)])
  }


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
     , Embed IO
     , FileProvider
     ] r


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


mapChars :: String -> [V.Key]
mapChars = fmap \case
  '←' -> V.KLeft
  '→' -> V.KRight
  '↓' -> V.KDown
  '↑' -> V.KUp
  c   -> V.KChar c


(-->) :: a -> b -> (a, b)
(-->) = (,)
infixr 0 -->

scroller :: Monad m => ViewportScroll Names m
scroller = viewportScroll Code


tactful :: Mems r=> Tactic r -> Data r -> Sem r (Data r)
tactful t st = st <$ runTacticOf t st

tactfulInvalid :: Mems r=> Tactic r -> Data r -> Sem r (Data r)
tactfulInvalid t st = do
  runTacticOf t st
  updateContext st


invalidating
  :: Mems r => (t -> Sem r (Data r)) -> t -> EventM Names (Sem r) (Next (Sem r) (Data r))
invalidating m st = do
  invalidateCacheEntry CodeCache
  M.performAction $ do
    st' <- m st
    updateContext st'


selecting
  :: Mems r => Traversal' LModule LExpr -> Data r -> EventM Names (Sem r) (Next (Sem r) (Data r))
selecting t st = do
  invalidateCacheEntry CodeCache
  M.continue $ st
    { dTarget = t
    }


purely :: Functor f => f b -> a -> f a
purely m st = st <$ m


continuing :: Monad m => EventM n m a -> s -> EventM n m (Next m s)
continuing m st = m >> continue st


sem :: Monad m => (t -> m s) -> t -> EventM n m (Next m s)
sem m st = do
  M.performAction $ do
    m st


prompt :: String -> (String -> Data r -> Sem r (Data r)) -> Data r -> Sem r (Data r)
prompt p f st = do
  withEdit st p $ \v st' ->
    f v st'

invalidateSuccess
  :: Mems r => Sem r (Maybe ()) -> Data r -> Sem r (Data r)
invalidateSuccess m st = m >>= maybe (pure st) (const $ updateContext st)


withEdit
    :: Data r
    -> String
    -> (String
        -> Data r
        -> Sem r (Data r))
    -> Sem r (Data r)
withEdit st p cont = pure $ st
  { dEditCont = Just (p, cont)
  , dEditor = resetEditor
  }


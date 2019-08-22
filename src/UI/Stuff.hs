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
import           Data.Monoid
import           GenericOrphans ()
import qualified Graphics.Vty as V
import           Name
import           Polysemy
import           Polysemy.Input
import           Polysemy.State
import           Sem.Anns
import           Sem.Fresh
import           Sem.Ghc
import           Sem.HoleInfo
import           Sem.Typecheck
import           Tactics
import qualified Trie as T
import           Types
import           Zipper


data Names = Editor | Code | CodeCache
  deriving (Eq, Ord, Show)

type Vim r = T.Trie V.Key (Last ( Data r -> T.EventM Names (Sem r) (T.Next (Sem r) (Data r))))


data Data r = Data
  { dEditCont :: Maybe (String, String -> Data r -> Sem r (Data r))
  , dEditor   :: Editor String Names
  , dDingus   :: Vim r
  , dCrampus  :: Vim r
  , dTarget   :: Traversal' LModule LExpr
  , dHoleInfo :: Maybe (Type, [(Name, Type)])
  }


defData :: Vim r -> Traversal' LModule LExpr -> Maybe (Type, [(Name, Type)]) -> Data r
defData v = Data Nothing resetEditor v v

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


invalidating
  :: Mems r => (t -> Sem r (Data r)) -> t -> EventM Names (Sem r) (Next (Sem r) (Data r))
invalidating m st = do
  invalidateCacheEntry CodeCache
  M.performAction $ do
    st' <- m st
    updateContext st'


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


module Bindings where

import           Control.Lens
import           Data.Data.Lens
import           Data.Generics
import qualified Data.Map as M
import qualified Data.Set as S
import           Id
import           SrcLoc
import           Types
import Bag
import Data.Traversable

splitLoc :: Located a -> (SrcSpan, a)
splitLoc (L a b) = (a, b)


bindings :: Data a => S.Set Id -> a -> M.Map SrcSpan (S.Set Id)
bindings in_scope = foldMapOf biplate $ cool collect
  where
    cool
        :: (HsExpr GhcTc -> M.Map SrcSpan (S.Set Id))
        -> LHsExpr GhcTc -> M.Map SrcSpan (S.Set Id)
    cool f (L src expr) = M.union (f expr) (M.singleton src in_scope)

    collect :: HsExpr GhcTc -> M.Map SrcSpan (S.Set Id)
    collect (HsLam _ matches)     = matchGroupBindings in_scope matches
    collect (HsLamCase _ matches) = matchGroupBindings in_scope matches
    collect (HsCase _ scrutinee matches) =
      M.union (bindings in_scope scrutinee) $ matchGroupBindings in_scope matches
    collect (HsLet _ (L _ binds) expr) =
      let (in_scope', res) = localBindsBindings in_scope binds
       in M.union (bindings in_scope' expr) res
    collect v = bindings in_scope v


matchGroupBindings :: S.Set Id -> MatchGroup GhcTc (LHsExpr GhcTc) -> M.Map SrcSpan (S.Set Id)
matchGroupBindings _ (XMatchGroup _) = M.empty
matchGroupBindings in_scope (MG _ (L _ alts) _) = M.fromList $ do
  L _ (Match _ _ pats body) <- alts
  let bound = everything S.union (mkQ S.empty S.singleton) pats
  M.toList $ bindings (S.union bound in_scope) body


localBindsBindings :: S.Set Id -> HsLocalBindsLR GhcTc GhcTc -> (S.Set Id, M.Map SrcSpan (S.Set Id))
localBindsBindings in_scope (HsValBinds _ (ValBinds _ binds _sigs)) =
  -- TODO(sandy): This doesn't do recursive binds
  flip foldMap (fmap unLoc $ bagToList binds) $ \case
    FunBind _ (L _ name) matches _ _ ->
      (S.singleton name, matchGroupBindings (S.insert name in_scope) matches)
    PatBind _ pat rhs _ ->
      let bound = everything S.union (mkQ S.empty S.singleton) pat
       in (bound, bindings (S.union bound in_scope) rhs)
    _ -> (mempty, mempty)
localBindsBindings s _  = (s, M.empty)


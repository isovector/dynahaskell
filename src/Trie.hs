{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor  #-}

module Trie where

import           Control.Comonad.Cofree
import           Data.Function
import qualified Data.Map as M
import           Data.Maybe
import           Data.Monoid
import           Prelude hiding (lookup)


newtype Trie k v = Trie { unTrie :: Cofree (M.Map k) v }
  deriving (Functor, Foldable)

instance (Ord k, Monoid v) => Semigroup (Trie k v) where
  Trie (a :< as) <> Trie (b :< bs) = Trie $
    a <> b :< M.unionWith ((unTrie .) . ((<>) `on` Trie)) as bs

instance (Ord k, Monoid v) => Monoid (Trie k v) where
  mempty = Trie $ mempty :< M.empty


singleton :: Monoid v => [k] -> v -> Trie k v
singleton ks v = Trie $
  unfold
    (\case
      [] -> (v, M.empty)
      (k:ks') -> (mempty, M.singleton k ks')
    ) ks


lookup :: Ord k => [k] -> Trie k v -> Maybe (v, Trie k v)
lookup [] t@(Trie (v :< _)) = pure (v, t)
lookup (k : ks) (Trie (_ :< t)) =
  case M.lookup k t of
    Just t' -> lookup ks $ Trie t'
    Nothing -> Nothing


follow :: Ord k => k -> Trie k v -> Maybe (v, Trie k v)
follow = lookup . pure


fromList :: (Ord k, Monoid v) => [([k], v)] -> Trie k v
fromList = foldMap (uncurry singleton)


data Vim k v = Vim
  { vimCurrent :: Trie k (Last v)
  , vimReset   :: Trie k (Last v)
  }
  deriving Functor


pump :: Ord k => Vim k v -> k -> (Maybe v, Vim k v)
pump (Vim c r) k =
  case follow k c of
    Just (Last v@(Just _), _) -> (v, Vim r r)
    Just (Last v, c')         -> (v, Vim c' r)
    Nothing                   -> (Nothing, Vim r r)


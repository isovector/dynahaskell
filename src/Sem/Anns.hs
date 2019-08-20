{-# LANGUAGE TemplateHaskell #-}

module Sem.Anns where

import Data.Foldable
import Control.Monad
import Control.Lens
import Polysemy
import Polysemy.State
import Language.Haskell.GHC.ExactPrint
import Types
import Data.Monoid
import Printers
import qualified Data.Map as M


data Anno m a where
  SpliceTree :: Traversal' LModule LExpr -> LExpr -> Anno m ()

makeSem ''Anno


runAnno
    :: Members '[State Anns, State LModule] r
    => Sem (Anno ': r) a
    -> Sem r a
runAnno
  = runEmbeddedTrans
  . reinterpret \case
      SpliceTree l e -> do
        t <- get @LModule

        -- create a unique srcspan for each result
        let n = length $ t ^.. l
        e' <- replicateM n $ embed $ ok e

        -- update the annotations so everything will ppr correctly
        modify @Anns $ (<> foldMap (\e0 -> addAnnotationsForPretty [] e0 mempty) e')
        put $ t & partsOf l .~ e'


runEmbeddedTrans
    :: Members '[State Anns] r
    => Sem (Embed Transform ': r) a
    -> Sem r a
runEmbeddedTrans
  = evalState @Int 0
  . reinterpret \case
      Embed m -> do
        i <- get
        anns <- get
        let (a, (anns', i'), _) = runTransformFrom i anns m
        put i'
        put anns'
        pure a


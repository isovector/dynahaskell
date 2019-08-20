{-# LANGUAGE TemplateHaskell #-}

module Sem.Anns where

import Control.Lens
import Control.Monad
import Language.Haskell.GHC.ExactPrint
import Polysemy
import Printers
import Sem.Fresh
import Types


data Anno m a where
  SpliceTree :: Traversal' LModule LExpr -> LExpr -> Source -> Anno m Source

makeSem ''Anno


runAnno
    :: Member (Fresh Integer) r
    => Sem (Anno ': r) a
    -> Sem r a
runAnno
  = interpret \case
      SpliceTree l e (Source anns t) -> do
        -- create a unique srcspan for each result
        let n = length $ t ^.. l
        e' <- replicateM n $ ok e

        -- update the annotations so everything will ppr correctly
        let anns' = anns <> foldMap (\e0 -> addAnnotationsForPretty [] e0 mempty) e'
        pure $ Source anns' $ t & partsOf l .~ e'


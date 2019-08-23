{-# LANGUAGE TemplateHaskell #-}

module Sem.Anns where

import Control.Lens
import Control.Monad
import Language.Haskell.GHC.ExactPrint
import Language.Haskell.GHC.ExactPrint.Annotater
import Polysemy
import Printers
import Sem.Fresh
import Types


data Anno m a where
  SpliceTree
      :: Annotate b
      => Traversal' LModule (Located b)
      -> Located b
      -> Source
      -> Anno m Source
  AppendNode
      :: Annotate b
      => Traversal' LModule [Located b]
      -> Located b
      -> Source
      -> Anno m Source

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


      AppendNode l e (Source anns t) -> do
        -- create a unique srcspan for each result
        let n = length $ t ^.. l
        e' <- replicateM n $ ok e

        -- update the annotations so everything will ppr correctly
        let anns' =
              mappend anns $ flip foldMap e' $ \e0 ->
                -- TODO(sandy): why this -1? because for some reason
                -- addAnnotationsForPretty will start new HsVars with a
                -- space.... and that throws off do alignment.
                setPrecedingLines e0 1 (-1) $ addAnnotationsForPretty [] e0 mempty
        pure $ Source anns' $ t & partsOf l %~ zipWith (flip snoc) e'


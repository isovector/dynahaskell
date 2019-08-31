
module EditorActions where

import Control.Lens hiding (holes)
import Data.Data.Lens
import Data.Traversable
import GenericOrphans ()
import Generics.SYB (everywhereM, mkM)
import HsExprUtils
import Language.Haskell.GHC.ExactPrint.Parsers (parseExpr, parseDecl)
import Language.Haskell.GHC.ExactPrint.Transform (setPrecedingLines)
import MarkerLenses
import MarkerUtils
import Outputable (ppr)
import Polysemy
import Polysemy.Input
import Printers
import Sem.Anns
import Sem.Fresh
import Types
import UI.Stuff
import Zipper


introduceTopLevel :: Mems r => String -> String -> Sem r (Maybe ())
introduceTopLevel nm ty = do
  decstr <- buildLDecl nm ty
  decs <- parseLDecl decstr
  for decs $ \(anns', ldecl) -> do
    Source anns t <- focus
    record $ Source (anns <> anns') $ t & loc . biplate <>~ [ldecl]


edit :: Mems r => Traversal' LModule LExpr -> String -> Sem r (Maybe ())
edit targ c = do
  z <- parseLExpr c
  for z $ \(anns', lexpr) -> do
    t <- focus
    Source anns t' <- spliceTree targ lexpr t
    record $ Source (anns <> anns') t'


------------------------------------------------------------------------------


doaction :: Mems r => Traversal' LModule [ExprLStmt GhcPs] -> Sem r ()
doaction targ = do
  td <- newTodo
  record
    =<< appendNode
          (taking 1 targ)
          (buildBodyStmt td)
    =<< focus


dobind :: Mems r => String -> Traversal' LModule [ExprLStmt GhcPs] -> Sem r ()
dobind nm targ = do
  td1 <- newTodo
  td2 <- newTodo
  record
    -- TODO(sandy): use doaction directly here
    =<< appendNode
          (taking 1 targ)
          (buildBodyStmt td2)
    =<< appendNode
          (taking 1 targ)
          (buildBindStmt nm td1)
    =<< focus


------------------------------------------------------------------------------


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


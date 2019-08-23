module HsExprUtils where

import Types


buildDoBlock :: [ExprLStmt GhcPs] -> LExpr
buildDoBlock = noLoc . HsDo noExt DoExpr . noLoc


buildBodyStmt :: LExpr -> ExprLStmt GhcPs
buildBodyStmt e = noLoc $ BodyStmt noExt e noSyntaxExpr noSyntaxExpr


buildBindStmt :: String -> LExpr -> ExprLStmt GhcPs
buildBindStmt nm e =
  noLoc
  $ BindStmt noExt
             (noLoc $ VarPat noExt $ noLoc $ Unqual $ mkVarOcc nm)
             e
             noSyntaxExpr
             noSyntaxExpr


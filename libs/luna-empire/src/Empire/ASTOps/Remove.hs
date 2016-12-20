{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Empire.ASTOps.Remove where

import           Empire.Prelude

import           Empire.Data.AST               (NodeRef)
import           Empire.ASTOp                  (ASTOp)

import Luna.IR.Expr.Combinators (deleteSubtree)


removeNode :: ASTOp m => NodeRef -> m ()
removeNode ref = deleteSubtree ref

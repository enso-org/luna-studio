{-# LANGUAGE LambdaCase #-}

module Empire.ASTOps.Remove (
    removeArg
  , removeSubtree
  ) where

import           Empire.Prelude

import           Empire.Data.AST          (NodeRef, NotAppException(..))
import           Empire.ASTOp             (ASTOp)

import           Luna.IR.Expr.Combinators (deleteSubtree)
import           Luna.IR.Expr.Term.Uni
import           Luna.IR (match)
import qualified Luna.IR as IR

removeSubtree :: ASTOp m => NodeRef -> m ()
removeSubtree ref = deleteSubtree ref

-- | Creates new App node with Blank inserted at specified position
removeArg :: ASTOp m => NodeRef -> Int -> m NodeRef
removeArg expr i = match expr $ \case
    App a c -> do
        nextApp <- IR.source a
        if i == 0 then do
            b  <- IR.blank
            IR.generalize <$> IR.app nextApp b
        else do
            d <- IR.source c
            f <- removeArg nextApp (i - 1)
            IR.generalize <$> IR.app f d
    _       -> throwM $ NotAppException expr

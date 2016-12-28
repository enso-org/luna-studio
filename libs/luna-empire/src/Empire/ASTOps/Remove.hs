{-# LANGUAGE LambdaCase #-}

module Empire.ASTOps.Remove where

import           Empire.Prelude

import           Empire.Data.AST          (NodeRef, NotAppException(..))
import           Empire.ASTOp             (ASTOp)

import           Luna.IR.Expr.Combinators (deleteSubtree)
import           Luna.IR.Expr.Term.Uni
import           Luna.IR.Function (arg)
import           Luna.IR.Function.Argument (Arg(..))
import           Luna.IR (match)
import qualified Luna.IR as IR

removeNode :: ASTOp m => NodeRef -> m ()
removeNode ref = deleteSubtree ref

removeArg :: ASTOp m => NodeRef -> Int -> m NodeRef
removeArg expr i = match expr $ \case
    App a (Arg _ c) -> do
        nextApp <- IR.source a
        if i == 0 then do
            b  <- IR.blank
            IR.generalize <$> IR.app nextApp (arg b)
        else do
            d <- IR.source c
            f <- removeArg nextApp (i - 1)
            IR.generalize <$> IR.app f (arg d)
    _       -> throwM $ NotAppException expr

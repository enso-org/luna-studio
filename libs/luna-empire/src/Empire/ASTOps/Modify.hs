{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-| This module contains operations that output modified nodes.
    These functions use reading, deconstructing and building APIs.

-}

module Empire.ASTOps.Modify (
    redirectLambdaOutput
  , renameVar
  , rewireNode
  , setLambdaOutputToBlank
  ) where

import           Empire.Prelude

import           Empire.API.Data.Node               (NodeId)
import           Empire.ASTOp                       (ASTOp)
import qualified Empire.ASTOps.Builder              as ASTBuilder
import qualified Empire.ASTOps.Deconstruct          as ASTDeconstruct
import qualified Empire.ASTOps.Read                 as ASTRead
import qualified Empire.ASTOps.Remove               as ASTRemove
import           Empire.Data.AST                    (NodeRef, NotLambdaException(..),
                                                     NotUnifyException(..))

import qualified Luna.IR.Expr.Combinators as IR (changeSource)
import           Luna.IR.Expr.Term.Uni
import           Luna.IR (match)
import qualified Luna.IR as IR



redirectLambdaOutput :: ASTOp m => NodeRef -> NodeRef -> m NodeRef
redirectLambdaOutput lambda newOutputRef = do
    match lambda $ \case
        Lam _args _ -> do
            args' <- ASTDeconstruct.extractArguments lambda
            ASTBuilder.lams args' newOutputRef
        _ -> throwM $ NotLambdaException lambda

setLambdaOutputToBlank :: ASTOp m => NodeRef -> m NodeRef
setLambdaOutputToBlank lambda = do
    match lambda $ \case
        Lam _args _ -> do
            args' <- ASTDeconstruct.extractArguments lambda
            blank <- IR.generalize <$> IR.blank
            ASTBuilder.lams args' blank
        _ -> throwM $ NotLambdaException lambda

replaceTargetNode :: ASTOp m => NodeRef -> NodeRef -> m ()
replaceTargetNode matchNode newTarget = do
    match matchNode $ \case
        Unify _l r -> do
            IR.changeSource r newTarget
        _ -> throwM $ NotUnifyException matchNode

rewireNode :: ASTOp m => NodeId -> NodeRef -> m ()
rewireNode nodeId newTarget = do
    matchNode <- ASTRead.getASTPointer nodeId
    oldTarget <- ASTRead.getASTTarget  nodeId
    replaceTargetNode matchNode newTarget
    ASTRemove.removeSubtree oldTarget

renameVar :: ASTOp m => NodeRef -> String -> m ()
renameVar vref name = match vref $ \case
    Var n -> do
        (var :: IR.Expr (IR.E IR.String)) <- IR.unsafeGeneralize <$> IR.source n
        IR.modifyExprTerm var $ IR.lit .~ name

{-# LANGUAGE LambdaCase #-}

{-| This module contains operations that output modified nodes.
    These functions use reading, deconstructing and building APIs.

-}

module Empire.ASTOps.Modify (
    redirectLambdaOutput
  , setLambdaOutputToBlank
  ) where

import           Empire.Prelude

import           Empire.ASTOp                       (ASTOp)
import           Empire.ASTOps.Builder              as ASTBuilder
import           Empire.ASTOps.Deconstruct          as ASTDeconstruct
import           Empire.Data.AST                    (NodeRef, NotLambdaException(..))

import Luna.IR.Expr.Term.Uni
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

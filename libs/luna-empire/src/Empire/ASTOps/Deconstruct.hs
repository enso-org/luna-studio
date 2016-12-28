{-# LANGUAGE LambdaCase #-}

module Empire.ASTOps.Deconstruct (
    deconstructApp
  , extractArguments
  ) where

import           Empire.Prelude

import           Empire.API.Data.Node               (NodeId)
import           Empire.ASTOp                       (ASTOp)
import           Empire.Data.AST                    (EdgeRef, NodeRef, NotAppException(..))
import           Empire.Data.Layers                 (NodeMarker(..), Marker)

import Luna.IR.Expr.Term.Uni
import Luna.IR.Function (arg)
import           Luna.IR.Function.Argument (Arg)
import qualified Luna.IR.Function.Argument as Arg
import           Luna.IR (match)
import qualified Luna.IR as IR


deconstructApp :: ASTOp m => NodeRef -> m (NodeRef, [NodeRef])
deconstructApp app' = match app' $ \case
    App a _ -> do
        unpackedArgs <- extractArguments app'
        target <- IR.source a
        return (target, unpackedArgs)
    _ -> throwM $ NotAppException app'

extractArguments :: ASTOp m => NodeRef -> m [NodeRef]
extractArguments expr = match expr $ \case
    App a (Arg.Arg _ b) -> do
        nextApp <- IR.source a
        args    <- extractArguments nextApp
        arg'    <- IR.source b
        return $ arg' : args
    Lam (Arg.Arg _ b) a -> do
        nextLam <- IR.source a
        args    <- extractArguments nextLam
        arg'    <- IR.source b
        return $ arg' : args
    _       -> return []

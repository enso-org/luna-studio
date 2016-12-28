{-# LANGUAGE LambdaCase #-}

module Empire.ASTOps.Deconstruct (
    deconstructApp
  , dumpArguments
    ) where

import           Empire.Prelude

import           Empire.API.Data.Node               (NodeId)
import           Empire.ASTOp                       (ASTOp)
import           Empire.ASTOps.Remove               (removeNode)
import           Empire.Data.AST                    (EdgeRef, NodeRef, NotAppException(..),
                                                     NotUnifyException(..), astExceptionFromException,
                                                     astExceptionToException)
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
        unpackedArgs <- dumpArguments app'
        target <- IR.source a
        return (target, unpackedArgs)
    _ -> throwM $ NotAppException app'

dumpArguments :: ASTOp m => NodeRef -> m [NodeRef]
dumpArguments expr = match expr $ \case
    App a (Arg.Arg _ b) -> do
        nextApp <- IR.source a
        args    <- dumpArguments nextApp
        arg'    <- IR.source b
        return $ arg' : args
    _       -> return []

{-# LANGUAGE LambdaCase #-}

module Empire.ASTOps.Deconstruct (
    deconstructApp
  , extractArguments
  , dumpAccessors
  ) where

import           Empire.Prelude

import           Empire.ASTOp                       (ASTOp)
import qualified Empire.ASTOps.Read                 as Read
import           Empire.Data.AST                    (NodeRef, NotAppException(..))

import           Luna.IR.Expr.Term.Uni
import           Luna.IR (match)
import qualified Luna.IR as IR


deconstructApp :: ASTOp m => NodeRef -> m (NodeRef, [NodeRef])
deconstructApp app' = match app' $ \case
    App a _ -> do
        unpackedArgs <- extractArguments app'
        target <- extractFun app'
        return (target, reverse unpackedArgs)
    _ -> throwM $ NotAppException app'

extractFun :: ASTOp m => NodeRef -> m NodeRef
extractFun app = match app $ \case
    App a _ -> do
        extractFun =<< IR.source a
    _ -> return app

extractArguments :: ASTOp m => NodeRef -> m [NodeRef]
extractArguments expr = match expr $ \case
    App a b -> do
        nextApp <- IR.source a
        args    <- extractArguments nextApp
        arg'    <- IR.source b
        return $ arg' : args
    Lam b a -> do
        nextLam <- IR.source a
        args    <- extractArguments nextLam
        arg'    <- IR.source b
        return $ arg' : args
    _       -> return []

dumpAccessors :: ASTOp m => NodeRef -> m (Maybe NodeRef, [String])
dumpAccessors = dumpAccessors' True

dumpAccessors' :: ASTOp m => Bool -> NodeRef -> m (Maybe NodeRef, [String])
dumpAccessors' firstApp node = do
    match node $ \case
        Var n -> do
            isNode <- Read.isGraphNode node
            name <- Read.getName n
            if isNode
                then return (Just node, [])
                else return (Nothing, [name])
        App t a -> do
            target <- IR.source t
            dumpAccessors' False target
        Acc n t -> do
            target <- IR.source t
            name <- Read.getName n
            (tgt, names) <- dumpAccessors' False target
            return (tgt, names ++ [name])
        _ -> return (Just node, [])

{-# LANGUAGE LambdaCase #-}

module Empire.ASTOps.Deconstruct (
    deconstructApp
  , extractArguments
  , dumpAccessors
  ) where

import           Empire.Prelude

import           Empire.ASTOp                       (ASTOp, match)
import qualified Empire.ASTOps.Read                 as Read
import           Empire.Data.AST                    (NodeRef, NotAppException(..))

import           Luna.IR.Term.Uni
import qualified Luna.IR as IR


deconstructApp :: ASTOp m => NodeRef -> m (NodeRef, [NodeRef])
deconstructApp app' = match app' $ \case
    App a _ -> do
        unpackedArgs <- extractArguments app'
        target <- extractFun app'
        return (target, unpackedArgs)
    _ -> throwM $ NotAppException app'

extractFun :: ASTOp m => NodeRef -> m NodeRef
extractFun app = match app $ \case
    App a _ -> do
        extractFun =<< IR.source a
    _ -> return app

data ExtractFilter = FApp | FLam

extractArguments :: ASTOp m => NodeRef -> m [NodeRef]
extractArguments expr = match expr $ \case
    App{}       -> reverse <$> extractArguments' FApp expr
    Lam{}       -> extractArguments' FLam expr
    Cons _ args -> mapM IR.source args
    _           -> return []

extractArguments' :: ASTOp m => ExtractFilter -> NodeRef -> m [NodeRef]
extractArguments' FApp expr = match expr $ \case
    App a b -> do
        nextApp <- IR.source a
        args    <- extractArguments' FApp nextApp
        arg'    <- IR.source b
        return $ arg' : args
    _       -> return []
extractArguments' FLam expr = match expr $ \case
    Lam b a -> do
        nextLam <- IR.source a
        args    <- extractArguments' FLam nextLam
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
            name <- Read.getVarName node
            if isNode
                then return (Just node, [])
                else return (Nothing, [name])
        App t a -> do
            target <- IR.source t
            dumpAccessors' False target
        Acc t n -> do
            target <- IR.source t
            let name = nameToString n
            (tgt, names) <- dumpAccessors' False target
            return (tgt, names ++ [name])
        _ -> return (Just node, [])

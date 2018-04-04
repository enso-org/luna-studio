{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}

module Empire.Commands.AST where

import           Control.Monad.State
import           Data.Function                     (on)
import           Data.List                         (sortBy)
import           Empire.Prelude

import           LunaStudio.Data.NodeId            (NodeId)
import           LunaStudio.Data.NodeMeta          (NodeMeta)
import           Empire.Data.AST                   (NodeRef, NotLambdaException(..))
import           Empire.Data.Layers                (Meta, SpanOffset, SpanLength)

import           Empire.ASTOp                      (ASTOp, GraphOp, match)
import qualified Empire.ASTOps.Builder             as ASTBuilder
import qualified Empire.ASTOps.Deconstruct         as ASTDeconstruct
import qualified Empire.ASTOps.Read                as ASTRead
import           Data.Text.Position                (Delta(..))

import qualified Luna.IR as IR
import           Luna.IR.Term.Uni
import qualified OCI.IR.Repr.Vis as Vis


addNode :: GraphOp m => NodeId -> Maybe Text -> m Text -> NodeRef -> m (NodeRef, Maybe Text)
addNode nid name genName node = do
    ASTBuilder.makeNodeRep nid name genName node

readMeta :: (IR.Reader IR.Layer (IR.AnyExpr IR.// Meta) m, IR.MonadRef m) => NodeRef -> m (Maybe NodeMeta)
readMeta ref = IR.getLayer @Meta ref

getNodeMeta :: GraphOp m => NodeId -> m (Maybe NodeMeta)
getNodeMeta = ASTRead.getASTRef >=> readMeta

writeMeta :: (IR.Writer IR.Layer (IR.AnyExpr IR.// Meta) m, IR.MonadRef m) => NodeRef -> NodeMeta -> m ()
writeMeta ref newMeta = IR.putLayer @Meta ref $ Just newMeta

sortByPosition :: GraphOp m => [NodeId] -> m [NodeRef]
sortByPosition nodeIds = do
    refs  <- mapM ASTRead.getASTPointer nodeIds
    metas <- mapM readMeta refs
    let refsAndMetas = zip refs metas
        sorted       = sortBy (compare `on` snd) refsAndMetas
    pure $ map (^. _1) sorted

makeSeq :: GraphOp m => [NodeRef] -> m (Maybe NodeRef)
makeSeq []     = pure Nothing
makeSeq [node] = pure $ Just node
makeSeq (n:ns) = Just <$> foldM f n ns
    where
        f :: GraphOp m => NodeRef -> NodeRef -> m NodeRef
        f l r = IR.generalize <$> IR.seq l r

readSeq :: GraphOp m => NodeRef -> m [NodeRef]
readSeq node = match node $ \case
    Seq l r -> do
        previous  <- IR.source l >>= readSeq
        rightmost <- IR.source r
        pure (previous <> [rightmost])
    _       -> pure [node]

getSeqs' :: GraphOp m => NodeRef -> m [NodeRef]
getSeqs' node = match node $ \case
    Seq l r -> do
        previous <- IR.source l >>= getSeqs'
        pure $ previous <> [node]
    _ -> pure []

getSeqs :: GraphOp m => NodeRef -> m [NodeRef]
getSeqs node = match node $ \case
    Seq l r -> do
        previous <- IR.source l >>= getSeqs'
        pure $ previous <> [node]
    _ -> pure [node]

previousNodeForSeq :: GraphOp m => NodeRef -> m (Maybe NodeRef)
previousNodeForSeq node = match node $ \case
    Seq l r -> do
        previousNode <- IR.source l
        match previousNode $ \case
            Seq l r -> Just <$> IR.source r
            _       -> pure $ Just previousNode
    _ -> pure Nothing

getLambdaInputRef :: GraphOp m => NodeRef -> Int -> m NodeRef
getLambdaInputRef node pos = do
    match node $ \case
        Grouped g      -> IR.source g >>= flip getLambdaInputRef pos
        Lam _args _out -> (!! pos) <$> ASTDeconstruct.extractArguments node
        _              -> throwM $ NotLambdaException node

isTrivialLambda :: GraphOp m => NodeRef -> m Bool
isTrivialLambda node = match node $ \case
    Grouped g -> IR.source g >>= isTrivialLambda
    Lam{} -> do
        args <- ASTDeconstruct.extractArguments node
        vars <- concat <$> mapM ASTRead.getVarsInside args
        out' <- ASTRead.getLambdaOutputRef node
        pure $ out' `elem` vars
    _ -> throwM $ NotLambdaException node

dumpGraphViz :: ASTOp g m => String -> m ()
dumpGraphViz name = Vis.snapshotWith nodeVis edgeVis name where
    edgeVis e = do
        off <- IR.getLayer @SpanOffset e
        pure $ Just $ convert $ "[" <> show (unwrap off) <> "]"
    nodeVis n = do
        len <- IR.getLayer @SpanLength n
        pure $ Just $ convert $ "[" <> show (unwrap len) <> "]"

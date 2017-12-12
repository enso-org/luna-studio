{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE OverloadedStrings #-}

module Empire.ASTOps.BreadcrumbHierarchy where

import Empire.Prelude

import           Control.Arrow                 ((&&&))
import           Control.Monad                 (forM)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Text.Position            (Delta)
import qualified Data.UUID.V4  as UUID

import           Empire.ASTOp                    (GraphOp)
import qualified Empire.ASTOps.Builder           as ASTBuilder
import qualified Empire.ASTOps.Deconstruct       as ASTDeconstruct
import qualified Empire.ASTOps.Read              as ASTRead
import qualified Empire.Commands.Code            as Code
import qualified Empire.Commands.AST             as AST
import           Empire.Data.AST                 (NodeRef, EdgeRef)
import qualified Empire.Data.BreadcrumbHierarchy as BH
import           Empire.Data.Graph               as Graph (NodeCache, nodeCache, breadcrumbHierarchy, nodeIdMap, nodeMetaMap)
import           Empire.Data.Layers              (Marker)
import           LunaStudio.Data.Node            (NodeId)
import           LunaStudio.Data.NodeLoc         (NodeLoc (..))
import           LunaStudio.Data.PortRef         (OutPortRef (..))
import qualified LunaStudio.Data.Port            as Port

import qualified Luna.IR as IR

makeTopBreadcrumbHierarchy :: GraphOp m => NodeRef -> m ()
makeTopBreadcrumbHierarchy ref = do
    item <- prepareFunctionChild ref ref
    breadcrumbHierarchy .= item

getMarker :: GraphOp m => NodeRef -> m Word64
getMarker marker = do
    IR.matchExpr marker $ \case
        IR.Marker index -> return index

childrenFromSeq :: GraphOp m => Delta -> EdgeRef -> m (Map NodeId BH.BChild)
childrenFromSeq tgtBeg edge = do
    ref <- IR.source edge
    off <- Code.getOffsetRelativeToTarget edge
    let beg = tgtBeg + off
    IR.matchExpr ref $ \case
        IR.Seq    l r    -> Map.union <$> (childrenFromSeq beg l) <*> (childrenFromSeq beg r)
        IR.Marked m expr -> do
            expr'      <- IR.source expr
            index      <- getMarker =<< IR.source m
            newNodeId  <- liftIO UUID.nextRandom
            nodeId     <- use $ Graph.nodeCache . nodeIdMap . at index
            let uid    = fromMaybe newNodeId nodeId
            childTarget <- IR.matchExpr expr' $ \case
                IR.Unify l r -> do
                    ASTBuilder.attachNodeMarkers uid []      =<< IR.source l
                    IR.source r
                _ -> do
                    IR.putLayer @Marker expr' $ Just $ OutPortRef (NodeLoc def uid) []
                    return expr'
            child    <- prepareChild ref childTarget
            nodeMeta <- use $ Graph.nodeCache . nodeMetaMap . at index
            forM nodeMeta $ AST.writeMeta ref
            return $ Map.singleton uid child
        _ -> do
            Code.addCodeMarker beg edge
            childrenFromSeq tgtBeg edge

isNone :: GraphOp m => NodeRef -> m Bool
isNone = flip IR.matchExpr $ \case
    IR.Cons n _ -> return $ n == "None"
    _           -> return False

lambdaChildren :: GraphOp m => Delta -> EdgeRef -> m (Map NodeId BH.BChild)
lambdaChildren tgtBeg edge = do
    ref <- IR.source edge
    off <- Code.getOffsetRelativeToTarget edge
    let beg = tgtBeg + off
    IR.matchExpr ref $ \case
        IR.Seq l r -> Map.union <$> (childrenFromSeq beg l) <*> (lambdaChildren beg r)
        _          -> do
            marker <- IR.getLayer @Marker ref
            isN    <- isNone ref
            if isJust marker || isN then return Map.empty else childrenFromSeq tgtBeg edge

prepareChild :: GraphOp m => NodeRef -> NodeRef -> m BH.BChild
prepareChild marked ref = go ref where
    go r = IR.matchExpr r $ \case
        IR.Lam {}         -> BH.LambdaChild <$> prepareLambdaChild   marked ref
        IR.ASGFunction {} -> BH.LambdaChild <$> prepareFunctionChild marked ref
        IR.Grouped g      -> go =<< IR.source g
        _                 -> prepareExprChild marked ref

prepareChildWhenLambda :: GraphOp m => NodeRef -> NodeRef -> m (Maybe BH.LamItem)
prepareChildWhenLambda marked ref = do
    isLambda <- ASTRead.isLambda ref
    if isLambda then Just <$> prepareLambdaChild marked ref else return Nothing

prepareLambdaChild :: GraphOp m => NodeRef -> NodeRef -> m BH.LamItem
prepareLambdaChild marked ref = do
    portMapping <- liftIO $ (,) <$> UUID.nextRandom <*> UUID.nextRandom
    Just lambdaBodyLink <- ASTRead.getFirstNonLambdaLink ref
    lambdaBody          <- IR.source lambdaBodyLink
    Just lambdaCodeBeg  <- Code.getOffsetRelativeToFile =<< IR.readTarget lambdaBodyLink
    ASTBuilder.attachNodeMarkersForArgs (fst portMapping) [] ref
    children            <- lambdaChildren lambdaCodeBeg lambdaBodyLink
    newBody             <- ASTRead.getFirstNonLambdaRef ref
    return $ BH.LamItem portMapping marked children

prepareFunctionChild :: GraphOp m => NodeRef -> NodeRef -> m BH.LamItem
prepareFunctionChild marked ref = do
    portMapping      <- liftIO $ (,) <$> UUID.nextRandom <*> UUID.nextRandom
    (args, bodyLink) <- IR.matchExpr ref $ \case
        IR.ASGFunction n as b -> do
            args <- mapM IR.source as
            return (args, b)
    body         <- IR.source bodyLink
    Just codeBeg <- Code.getOffsetRelativeToFile ref
    for_ (zip args [0..]) $ \(a, i) -> ASTBuilder.attachNodeMarkers (fst portMapping) [Port.Projection i] a
    children <- lambdaChildren codeBeg bodyLink
    newBody  <- ASTRead.getFirstNonLambdaRef ref
    return $ BH.LamItem portMapping marked children

prepareExprChild :: GraphOp m => NodeRef -> NodeRef -> m BH.BChild
prepareExprChild marked ref = do
    let bareItem = BH.ExprItem Map.empty marked
    args  <- ASTDeconstruct.extractAppArguments ref
    items <- mapM (uncurry (prepareChildWhenLambda) . (id &&& id)) args
    let addItem par (port, child) = case child of
          Just ch -> par & BH.portChildren . at port ?~ ch
          _       -> par
    return $ BH.ExprChild $ foldl addItem bareItem $ zip [0..] items

restorePortMappings :: GraphOp m => Map (NodeId, Maybe Int) (NodeId, NodeId) -> m ()
restorePortMappings previousPortMappings = do
    hierarchy <- use breadcrumbHierarchy

    let goParent lamItem = goLamItem Nothing lamItem

        goBChild nodeId (BH.ExprChild exprItem)  = BH.ExprChild <$> goExprItem nodeId exprItem
        goBChild nodeId (BH.LambdaChild lamItem) = BH.LambdaChild <$> goLamItem (Just (nodeId, Nothing)) lamItem

        goLamItem idArg (BH.LamItem mapping marked children) = do
            let cache       = case idArg of
                    Just idarg -> Map.lookup idarg previousPortMappings
                    _          -> Nothing
            forM cache $ \prev -> do
                ref <- ASTRead.getTargetFromMarked marked
                ASTBuilder.attachNodeMarkersForArgs (fst prev) [] ref
            updatedChildren <- do
                updatedChildren <- mapM (\(a, b) -> (a,) <$> goBChild a b) $ Map.assocs children
                return $ Map.fromList updatedChildren
            return $ BH.LamItem (fromMaybe mapping cache) marked updatedChildren

        goExprItem nodeId (BH.ExprItem children self) = do
            updatedChildren <- mapM (\(a, b) -> (a,) <$> goLamItem (Just (nodeId, Just a)) b) $ Map.assocs children
            return $ BH.ExprItem (Map.fromList updatedChildren) self

    newHierarchy <- goParent hierarchy
    breadcrumbHierarchy .= newHierarchy

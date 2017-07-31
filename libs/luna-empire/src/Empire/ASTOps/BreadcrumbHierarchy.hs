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
import qualified Data.UUID.V4  as UUID

import           Empire.ASTOp                    (GraphOp)
import qualified Empire.ASTOps.Builder           as ASTBuilder
import qualified Empire.ASTOps.Deconstruct       as ASTDeconstruct
import qualified Empire.ASTOps.Read              as ASTRead
import           Empire.Commands.Code            (addCodeMarker)
import qualified Empire.Commands.AST             as AST
import           Empire.Data.AST                 (NodeRef)
import qualified Empire.Data.BreadcrumbHierarchy as BH
import           Empire.Data.Graph               as Graph (NodeCache, breadcrumbHierarchy, nodeIdMap, nodeMetaMap)
import           Empire.Data.Layers              (Marker)
import           LunaStudio.Data.Node            (NodeId)
import           LunaStudio.Data.NodeLoc         (NodeLoc (..))
import           LunaStudio.Data.PortRef         (OutPortRef (..))
import qualified LunaStudio.Data.Port            as Port

import qualified Luna.IR as IR

makeTopBreadcrumbHierarchy :: GraphOp m => NodeCache -> NodeRef -> m ()
makeTopBreadcrumbHierarchy nodeCache ref = do
    item <- prepareLambdaChild nodeCache ref ref
    breadcrumbHierarchy .= item

getMarker :: GraphOp m => NodeRef -> m Word64
getMarker marker = do
    IR.matchExpr marker $ \case
        IR.Marker index -> return index

childrenFromSeq :: GraphOp m => NodeCache -> NodeRef -> m (Map NodeId BH.BChild)
childrenFromSeq nodeCache ref = do
    IR.matchExpr ref $ \case
        IR.Seq    l r    -> Map.union <$> (childrenFromSeq nodeCache =<< IR.source l) <*> (childrenFromSeq nodeCache =<< IR.source r)
        IR.Marked m expr -> do
            expr'      <- IR.source expr
            index      <- getMarker =<< IR.source m
            newNodeId  <- liftIO UUID.nextRandom
            let nodeId = nodeCache ^. nodeIdMap . at index
                uid    = fromMaybe newNodeId nodeId
            childTarget <- IR.matchExpr expr' $ \case
                IR.Unify l r -> do
                    ASTBuilder.attachNodeMarkers uid []      =<< IR.source l
                    IR.source r
                _ -> do
                    IR.putLayer @Marker expr' $ Just $ OutPortRef (NodeLoc def uid) []
                    return expr'
            child <- prepareChild nodeCache ref childTarget
            forM (nodeCache ^? Graph.nodeMetaMap . ix index) $ AST.writeMeta ref
            return $ Map.singleton uid child
        _ -> childrenFromSeq nodeCache =<< addCodeMarker ref

isNone :: GraphOp m => NodeRef -> m Bool
isNone = flip IR.matchExpr $ \case
    IR.Cons n _ -> return $ n == "None"
    _           -> return False

lambdaChildren :: GraphOp m => NodeCache -> NodeRef -> m (Map NodeId BH.BChild)
lambdaChildren nodeCache ref = IR.matchExpr ref $ \case
    IR.Seq l r -> Map.union <$> (childrenFromSeq nodeCache =<< IR.source l) <*> (lambdaChildren nodeCache =<< IR.source r)
    _          -> do
        marker <- IR.getLayer @Marker ref
        isN    <- isNone ref
        if isJust marker || isN then return Map.empty else childrenFromSeq nodeCache ref

prepareChild :: GraphOp m => NodeCache -> NodeRef -> NodeRef -> m BH.BChild
prepareChild nodeCache marked ref = go ref where
    go r = IR.matchExpr r $ \case
        IR.Lam {}         -> BH.LambdaChild <$> prepareLambdaChild   nodeCache marked ref
        IR.ASGFunction {} -> BH.LambdaChild <$> prepareFunctionChild nodeCache marked ref
        IR.Grouped g      -> go =<< IR.source g
        _                 -> prepareExprChild nodeCache marked ref

prepareChildWhenLambda :: GraphOp m => NodeCache -> NodeRef -> NodeRef -> m (Maybe BH.LamItem)
prepareChildWhenLambda nodeCache marked ref = do
    isLambda <- ASTRead.isLambda ref
    if isLambda then Just <$> prepareLambdaChild nodeCache marked ref else return Nothing

prepareLambdaChild :: GraphOp m => NodeCache -> NodeRef -> NodeRef -> m BH.LamItem
prepareLambdaChild nodeCache marked ref = do
    portMapping <- liftIO $ (,) <$> UUID.nextRandom <*> UUID.nextRandom
    lambdaBody  <- ASTRead.getFirstNonLambdaRef ref
    ASTBuilder.attachNodeMarkersForArgs (fst portMapping) [] ref
    oldBH       <- use Graph.breadcrumbHierarchy
    let workingBH = BH.LamItem portMapping marked def lambdaBody
    Graph.breadcrumbHierarchy .= workingBH
    children    <- lambdaChildren nodeCache lambdaBody
    BH.LamItem mapping self _ body <- use Graph.breadcrumbHierarchy
    Graph.breadcrumbHierarchy .= oldBH
    return $ BH.LamItem mapping self children body

prepareFunctionChild :: GraphOp m => NodeCache -> NodeRef -> NodeRef -> m BH.LamItem
prepareFunctionChild nodeCache marked ref = do
    portMapping  <- liftIO $ (,) <$> UUID.nextRandom <*> UUID.nextRandom
    (args, body) <- IR.matchExpr ref $ \case
        IR.ASGFunction n as b -> do
            body <- IR.source b
            args <- mapM IR.source as
            return (args, body)
    forM_ (zip args [0..]) $ \(a, i) -> ASTBuilder.attachNodeMarkers (fst portMapping) [Port.Projection i] a
    oldBH <- use Graph.breadcrumbHierarchy
    let workingBH = BH.LamItem portMapping marked def body
    Graph.breadcrumbHierarchy .= workingBH
    children <- lambdaChildren nodeCache body
    BH.LamItem mapping _ _ body <- use Graph.breadcrumbHierarchy
    Graph.breadcrumbHierarchy .= oldBH
    return $ BH.LamItem mapping marked children body

prepareExprChild :: GraphOp m => NodeCache -> NodeRef -> NodeRef -> m BH.BChild
prepareExprChild nodeCache marked ref = do
    let bareItem = BH.ExprItem Map.empty marked
    args  <- ASTDeconstruct.extractAppArguments ref
    items <- mapM (uncurry (prepareChildWhenLambda nodeCache) . (id &&& id)) args
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

        goLamItem idArg (BH.LamItem mapping marked children body) = do
            let cache       = case idArg of
                    Just idarg -> Map.lookup idarg previousPortMappings
                    _          -> Nothing
            forM cache $ \prev -> do
                ref <- ASTRead.getTargetFromMarked marked
                ASTBuilder.attachNodeMarkersForArgs (fst prev) [] ref
            updatedChildren <- do
                updatedChildren <- mapM (\(a, b) -> (a,) <$> goBChild a b) $ Map.assocs children
                return $ Map.fromList updatedChildren
            return $ BH.LamItem (fromMaybe mapping cache) marked updatedChildren body

        goExprItem nodeId (BH.ExprItem children self) = do
            updatedChildren <- mapM (\(a, b) -> (a,) <$> goLamItem (Just (nodeId, Just a)) b) $ Map.assocs children
            return $ BH.ExprItem (Map.fromList updatedChildren) self

    newHierarchy <- goParent hierarchy
    breadcrumbHierarchy .= newHierarchy

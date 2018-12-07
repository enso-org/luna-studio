{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE OverloadedStrings #-}

module Empire.ASTOps.BreadcrumbHierarchy where

import Empire.Prelude hiding (seq)

import           Control.Arrow                 ((&&&))
import           Control.Monad                 (forM)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Text.Position            (Delta)
import qualified Data.UUID.V4  as UUID

import           Empire.ASTOp                    (ASTOp, GraphOp)
import qualified Empire.ASTOps.Builder           as ASTBuilder
import qualified Empire.ASTOps.Deconstruct       as ASTDeconstruct
import qualified Empire.ASTOps.Read              as ASTRead
import qualified Empire.Commands.Code            as Code
import qualified Empire.Commands.AST             as AST
import           Empire.Data.AST                 (NodeRef, EdgeRef)
import qualified Empire.Data.BreadcrumbHierarchy as BH
import           Empire.Data.Graph               as Graph (nodeCache, breadcrumbHierarchy)
import           Empire.Data.Layers              (Marker)
import           LunaStudio.Data.NodeId          (NodeId)
import           LunaStudio.Data.NodeCache       (nodeIdMap, nodeMetaMap)
import           LunaStudio.Data.PortRef         (OutPortRef (..))
import qualified LunaStudio.Data.Port            as Port


makeTopBreadcrumbHierarchy :: NodeRef -> GraphOp ()
makeTopBreadcrumbHierarchy ref = do

    item <- prepareFunctionChild ref ref
    breadcrumbHierarchy .= item

getMarker :: NodeRef -> ASTOp g Word64
getMarker marker = do
    matchExpr marker $ \case
        Marker index -> return index

childrenFromSeq :: NodeRef -> Delta -> EdgeRef -> GraphOp (Map NodeId BH.BChild)
childrenFromSeq currentFun tgtBeg edge = do
    ref <- source edge
    off <- Code.getOffsetRelativeToTarget edge
    let beg = tgtBeg + off
    matchExpr ref $ \case
        Seq    l r    -> Map.union
            <$> childrenFromSeq currentFun beg (coerce l)
            <*> lambdaChildren  currentFun beg (coerce r)
        Marked m expr -> do
            expr'      <- source expr
            index      <- getMarker =<< source m
            newNodeId  <- liftIO UUID.nextRandom
            nodeId     <- use $ Graph.nodeCache . nodeIdMap . at index
            let uid    = fromMaybe newNodeId nodeId
            childTarget <- matchExpr expr' $ \case
                Unify l r -> do
                    ASTBuilder.attachNodeMarkers uid [] =<< source l
                    source r
                ASGFunction n _ _ -> do
                    ASTBuilder.attachNodeMarkers uid [] =<< source n
                    pure expr'
                _ -> do
                    putLayer @Marker expr' . Just
                        =<< toPortMarker (OutPortRef (convert uid) [])
                    pure expr'
            child    <- prepareChild ref childTarget
            nodeMeta <- use $ Graph.nodeCache . nodeMetaMap . at index
            forM nodeMeta $ AST.writeMeta ref
            pure $ Map.singleton uid child
        Invalid{} -> pure def
        _ -> do
            void $ Code.addCodeMarker beg edge
            childrenFromSeq currentFun tgtBeg edge

lambdaChildren :: NodeRef -> Delta -> EdgeRef -> GraphOp (Map NodeId BH.BChild)
lambdaChildren currentFun tgtBeg edge = do
    ref <- source edge
    off <- Code.getOffsetRelativeToTarget edge
    let beg = tgtBeg + off
    matchExpr ref $ \case
        Seq l r -> Map.union
            <$> childrenFromSeq currentFun beg (coerce l)
            <*> lambdaChildren  currentFun beg (coerce r)
        Var{}   -> do
            output <- ASTRead.getLambdaOutputRef currentFun
            if output == ref
            then do
                marker <- getLayer @Marker ref
                if isJust marker
                then pure Map.empty
                else childrenFromSeq currentFun tgtBeg edge
            else do
                tgt    <- target edge
                seq    <- ASTRead.isSeq tgt
                if seq
                then childrenFromSeq currentFun tgtBeg edge
                else pure Map.empty
        _          -> do
            marker <- getLayer @Marker ref
            none   <- ASTRead.isNone ref
            if isJust marker || none
            then pure Map.empty
            else childrenFromSeq currentFun tgtBeg edge

prepareChild :: NodeRef -> NodeRef -> GraphOp BH.BChild
prepareChild marked ref = go ref where
    go r = matchExpr r $ \case
        Lam {}         -> BH.LambdaChild <$> prepareLambdaChild   marked ref
        ASGFunction {} -> BH.LambdaChild <$> prepareFunctionChild marked ref
        Grouped g      -> go =<< source g
        _              -> prepareExprChild marked ref

prepareChildWhenLambda :: NodeRef -> NodeRef -> GraphOp (Maybe BH.LamItem)
prepareChildWhenLambda marked ref = do
    isLambda <- ASTRead.isLambda ref
    if isLambda then Just <$> prepareLambdaChild marked ref else return Nothing

prepareLambdaChild :: NodeRef -> NodeRef -> GraphOp BH.LamItem
prepareLambdaChild marked ref = do
    portMapping <- liftIO $ (,) <$> UUID.nextRandom <*> UUID.nextRandom
    Just lambdaBodyLink <- ASTRead.getFirstNonLambdaLink ref
    Just lambdaCodeBeg  <- Code.getOffsetRelativeToFile =<< target lambdaBodyLink
    ASTBuilder.attachNodeMarkersForArgs (fst portMapping) [] ref
    children            <- lambdaChildren ref lambdaCodeBeg lambdaBodyLink
    return $ BH.LamItem portMapping marked children

prepareFunctionChild :: NodeRef -> NodeRef -> GraphOp BH.LamItem
prepareFunctionChild marked ref = do
    portMapping      <- liftIO $ (,) <$> UUID.nextRandom <*> UUID.nextRandom
    (args, bodyLink) <- matchExpr ref $ \case
        ASGFunction _ as b -> do
            args <- mapM source =<< ptrListToList as
            return (args, coerce b)
    Just codeBeg <- Code.getOffsetRelativeToFile ref
    for_ (zip args [0..]) $ \(a, i) ->
        ASTBuilder.attachNodeMarkers (fst portMapping) [Port.Projection i] a
    children <- lambdaChildren marked codeBeg bodyLink
    return $ BH.LamItem portMapping marked children

prepareExprChild :: NodeRef -> NodeRef -> GraphOp BH.BChild
prepareExprChild marked ref = do
    let bareItem = BH.ExprItem Map.empty marked
    args  <- ASTDeconstruct.extractAppArguments ref
    items <- mapM (uncurry (prepareChildWhenLambda) . (id &&& id)) args
    let addItem par (port, child) = case child of
          Just ch -> par & BH.portChildren . at port ?~ ch
          _       -> par
    return $ BH.ExprChild $ foldl addItem bareItem $ zip [0..] items

restorePortMappings :: NodeId -> Map (NodeId, Maybe Int) (NodeId, NodeId) -> GraphOp ()
restorePortMappings funId previousPortMappings = do
    hierarchy <- use breadcrumbHierarchy

    let goParent lamItem = goLamItem (Just (funId, Nothing)) lamItem

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

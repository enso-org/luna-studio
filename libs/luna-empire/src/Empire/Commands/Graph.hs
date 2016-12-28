{-# LANGUAGE MultiWayIf       #-}
{-# LANGUAGE TypeApplications #-}

module Empire.Commands.Graph
    ( addNode
    , addNodeCondTC
    , addPersistentNode
    , addSubgraph
    , removeNodes
    , updateNodeExpression
    , updateNodeMeta
    , connect
    , connectPersistent
    , connectCondTC
    , connectNoTC
    , decodeLocation
    , disconnect
    , getNodeMeta
    , getCode
    , getGraph
    , getNodes
    , getConnections
    , setDefaultValue
    , renameNode
    , dumpGraphViz
    , typecheck
    , withTC
    , withGraph
    ) where

import           Control.Monad                 (forM, forM_)
import           Control.Monad.State           hiding (when)
import           Data.Coerce                   (coerce)
import           Data.List                     (sort)
import qualified Data.Map                      as Map
import           Data.Text.Lazy                (Text)
import qualified Data.Text.Lazy                as Text
import qualified Data.UUID                     as UUID
import qualified Data.UUID.V4                  as UUID (nextRandom)
import           Empire.Prelude

import           Empire.Data.BreadcrumbHierarchy (addID, addWithLeafs, removeID, topLevelIDs)
import           Empire.Data.Graph               (Graph)
import qualified Empire.Data.Graph               as Graph
import           Empire.Data.Layers              (Marker, NodeMarker(..))

import           Empire.API.Data.Breadcrumb      as Breadcrumb (Breadcrumb(..), Named, BreadcrumbItem(..))
import           Empire.API.Data.Connection      (Connection (..))
import           Empire.API.Data.DefaultValue    (PortDefault (Constant))
import qualified Empire.API.Data.Graph           as APIGraph
import           Empire.API.Data.GraphLocation   (GraphLocation (..))
import           Empire.API.Data.Node            (Node (..), NodeId)
import qualified Empire.API.Data.Node            as Node
import           Empire.API.Data.NodeMeta        (NodeMeta)
import qualified Empire.API.Data.NodeMeta        as NodeMeta
import           Empire.API.Data.Port            (InPort (..), OutPort (..), PortId (..))
import qualified Empire.API.Data.Port            as Port (PortState (..), state)
import           Empire.API.Data.PortRef         (AnyPortRef (..), InPortRef (..), OutPortRef (..))
import qualified Empire.API.Data.PortRef         as PortRef
import           Empire.ASTOp                    (ASTOp, runASTOp)

import qualified Empire.ASTOps.Read              as ASTRead
import qualified Empire.ASTOps.Remove            as ASTRemove
import qualified Empire.Commands.AST             as AST
import           Empire.Commands.Breadcrumb      (withBreadcrumb)
import qualified Empire.Commands.GraphBuilder    as GraphBuilder
import qualified Empire.Commands.GraphUtils      as GraphUtils
import qualified Empire.Commands.Publisher       as Publisher
import           Empire.Empire

import qualified Luna.IR as IR

generateNodeName :: ASTOp m => m String
generateNodeName = do
    lastNameId <- use Graph.lastNameId
    let newNameId = lastNameId + 1
    Graph.lastNameId .= newNameId
    return $ "node" <> show newNameId

addNodeCondTC :: Bool -> GraphLocation -> NodeId -> Text -> NodeMeta -> Empire Node
addNodeCondTC doTC loc uuid expr meta = withGraph loc $ do
    node <- addNodeNoTC loc uuid expr meta
    when doTC $ runTC loc False
    return node

addNode :: GraphLocation -> NodeId -> Text -> NodeMeta -> Empire Node
addNode loc uuid expr meta = withTC loc False $ addNodeNoTC loc uuid expr meta

addNodeNoTC :: GraphLocation -> NodeId -> Text -> NodeMeta -> Command Graph Node
addNodeNoTC loc uuid expr meta = do
    node <- runASTOp $ do
        newNodeName <- generateNodeName
        (parsedRef, refNode) <- AST.addNode uuid newNodeName (Text.unpack expr)
        parsedIsLambda <- ASTRead.isLambda parsedRef
        AST.writeMeta refNode meta
        Graph.nodeMapping . at uuid ?= Graph.MatchNode refNode
        node <- GraphBuilder.buildNode uuid
        if parsedIsLambda then do
            lambdaUUID <- liftIO $ UUID.nextRandom
            lambdaOutput <- AST.getLambdaOutputRef parsedRef
            outputIsOneOfTheInputs <- AST.isTrivialLambda parsedRef
            when (not outputIsOneOfTheInputs) $ Graph.nodeMapping . at lambdaUUID ?= Graph.AnonymousNode lambdaOutput
            Graph.breadcrumbHierarchy %= addWithLeafs (node ^. Node.nodeId)
                (if outputIsOneOfTheInputs then [] else [lambdaUUID])
            IR.writeLayer @Marker (Just $ NodeMarker lambdaUUID) lambdaOutput
        else Graph.breadcrumbHierarchy %= addID (node ^. Node.nodeId)
        return node
    Publisher.notifyNodeUpdate loc node
    return node

addPersistentNode :: ASTOp m => Node -> m NodeId
addPersistentNode n = case n ^. Node.nodeType of
    Node.ExpressionNode expr -> do
        let newNodeId = n ^. Node.nodeId
        (parsedRef, refNode) <- AST.addNode newNodeId (Text.unpack $ n ^. Node.name) (Text.unpack expr)
        AST.writeMeta refNode (n ^. Node.nodeMeta)
        Graph.nodeMapping . at newNodeId ?= Graph.MatchNode refNode
        lambdaUUID <- liftIO $ UUID.nextRandom
        Graph.nodeMapping . at lambdaUUID ?= Graph.AnonymousNode parsedRef
        mapM_ (setDefault newNodeId) (Map.toList $ n ^. Node.ports)
        return newNodeId
    _ -> return UUID.nil
    where
        setDefault nodeId (portId, port) = case port ^. Port.state of
            Port.WithDefault (Constant val) -> case portId of
                (InPortId pid) -> setDefaultValue' (PortRef.toAnyPortRef nodeId (InPortId pid)) (Constant val)
                _ -> return ()
            _ -> return ()

addSubgraph :: GraphLocation -> [Node] -> [Connection] -> Empire ()
addSubgraph loc nodes conns = withTC loc False $ do
    forM_ nodes $ \n -> case n ^. Node.nodeType of
        Node.ExpressionNode expr -> void $ addNodeNoTC loc (n ^. Node.nodeId) expr (n ^. Node.nodeMeta)
        _ -> return ()
    forM_ conns $ \(Connection src dst) -> connectNoTC loc src dst

descendInto :: GraphLocation -> NodeId -> GraphLocation
descendInto (GraphLocation pid lid breadcrumb) nid = GraphLocation pid lid breadcrumb'
    where
        breadcrumb' = coerce $ coerce breadcrumb ++ [Breadcrumb.Lambda nid]

removeNodes :: GraphLocation -> [NodeId] -> Empire ()
removeNodes loc nodeIds = do
    forM_ nodeIds $ \nodeId -> do
        children <- withTC (loc `descendInto` nodeId) False $ do
            uses Graph.breadcrumbHierarchy topLevelIDs
        removeNodes (loc `descendInto` nodeId) children
    withTC loc False $ runASTOp $ forM_ nodeIds removeNodeNoTC

removeNodeNoTC :: ASTOp m => NodeId -> m ()
removeNodeNoTC nodeId = do
    astRef <- GraphUtils.getASTPointer nodeId
    obsoleteEdges <- getOutEdges nodeId
    mapM_ disconnectPort obsoleteEdges
    AST.removeSubtree astRef
    Graph.nodeMapping %= Map.delete nodeId
    Graph.breadcrumbHierarchy %= removeID nodeId

updateNodeExpression :: GraphLocation -> NodeId -> NodeId -> Text -> Empire (Maybe Node)
updateNodeExpression loc nodeId newNodeId expr = do
    metaMay <- withGraph loc $ runASTOp $ do
        ref <- GraphUtils.getASTPointer nodeId
        AST.readMeta ref
    forM metaMay $ \meta ->
        withTC loc False $ do
            runASTOp $ removeNodeNoTC nodeId
            addNodeNoTC loc newNodeId expr meta

updateNodeMeta :: GraphLocation -> NodeId -> NodeMeta -> Empire ()
updateNodeMeta loc nodeId newMeta = withGraph loc $ do
    doTCMay <- runASTOp $ do
        ref <- GraphUtils.getASTPointer nodeId
        oldMetaMay <- AST.readMeta ref
        doTCMay <- forM oldMetaMay $ \oldMeta ->
            return $ triggerTC oldMeta newMeta
        AST.writeMeta ref newMeta
        return doTCMay
    forM_ doTCMay $ \doTC ->
        when doTC $ runTC loc False
    where
        triggerTC :: NodeMeta -> NodeMeta -> Bool
        triggerTC oldMeta' newMeta' = oldMeta' ^. NodeMeta.displayResult /= newMeta' ^. NodeMeta.displayResult

connectCondTC :: Bool -> GraphLocation -> OutPortRef -> InPortRef -> Empire ()
connectCondTC doTC loc outPort inPort = withGraph loc $ do
    connectNoTC loc outPort inPort
    when doTC $ runTC loc False

connect :: GraphLocation -> OutPortRef -> InPortRef -> Empire ()
connect loc outPort inPort = withTC loc False $ connectNoTC loc outPort inPort

connectPersistent :: ASTOp m => OutPortRef -> InPortRef -> m ()
connectPersistent (OutPortRef srcNodeId srcPort) (InPortRef dstNodeId dstPort) = do
    let inputPos = case srcPort of
            All            -> 0   -- FIXME: do not equalise All with Projection 0
            Projection int -> int
    case dstPort of
        Self    -> makeAcc srcNodeId dstNodeId inputPos
        Arg num -> makeApp srcNodeId dstNodeId num inputPos

connectNoTC :: GraphLocation -> OutPortRef -> InPortRef -> Command Graph ()
connectNoTC _loc outPort inPort = runASTOp $ connectPersistent outPort inPort

setDefaultValue :: GraphLocation -> AnyPortRef -> PortDefault -> Empire ()
setDefaultValue loc portRef val = withTC loc False $ runASTOp $ setDefaultValue' portRef val

setDefaultValue' :: ASTOp m => AnyPortRef -> PortDefault -> m ()
setDefaultValue' portRef val = do
    parsed <- AST.addDefault val
    (nodeId, newRef) <- case portRef of
        InPortRef' (InPortRef nodeId port) -> do
            ref <- GraphUtils.getASTTarget nodeId
            newRef <- case port of
                Self    -> AST.makeAccessor parsed ref
                Arg num -> AST.applyFunction ref parsed num
            return (nodeId, newRef)
        OutPortRef' (OutPortRef nodeId _) -> return (nodeId, parsed)
    GraphUtils.rewireNode nodeId newRef

disconnect :: GraphLocation -> InPortRef -> Empire ()
disconnect loc port = withTC loc False $ runASTOp $ disconnectPort port

getNodeMeta :: GraphLocation -> NodeId -> Empire (Maybe NodeMeta)
getNodeMeta loc nodeId = withGraph loc $ runASTOp $ do
    ref <- GraphUtils.getASTPointer nodeId
    AST.readMeta ref

getCode :: GraphLocation -> Empire String
getCode loc = withGraph loc $ runASTOp $ do
    inFunction <- use Graph.insideNode
    function <- forM inFunction $ \nodeId -> do
        ptr <- GraphUtils.getASTPointer nodeId
        header <- AST.printFunctionHeader ptr
        lam <- GraphUtils.getASTTarget nodeId
        ret <- AST.printReturnValue lam
        return (header, ret)
    returnedNodeId <- GraphBuilder.nodeConnectedToOutput
    allNodes <- uses Graph.breadcrumbHierarchy topLevelIDs
    refs     <- mapM GraphUtils.getASTPointer $ flip filter allNodes $ \nid ->
        case returnedNodeId of
            Just id' -> id' /= nid
            _       -> True
    metas    <- mapM AST.readMeta refs
    let sorted = fmap snd $ sort $ zip metas allNodes
    lines' <- mapM printNodeLine sorted
    return $ unlines $ case function of
        Just (header, ret) -> header : map ("    " ++) (lines' ++ [ret])
        _                  -> lines'

getGraph :: GraphLocation -> Empire APIGraph.Graph
getGraph loc = withTC loc True $ runASTOp GraphBuilder.buildGraph

getNodes :: GraphLocation -> Empire [Node]
getNodes loc = withTC loc True $ runASTOp $ view APIGraph.nodes <$> GraphBuilder.buildGraph

getConnections :: GraphLocation -> Empire [(OutPortRef, InPortRef)]
getConnections loc = withTC loc True $ runASTOp $ view APIGraph.connections <$> GraphBuilder.buildGraph

decodeLocation :: GraphLocation -> Empire (Breadcrumb (Named BreadcrumbItem))
decodeLocation loc@(GraphLocation _ _ crumbs) = withGraph loc $ GraphBuilder.decodeBreadcrumbs crumbs

renameNode :: GraphLocation -> NodeId -> Text -> Empire ()
renameNode loc nid name = withTC loc False $ runASTOp $ do
    vref <- GraphUtils.getASTVar nid
    AST.renameVar vref (Text.unpack name)

dumpGraphViz :: GraphLocation -> Empire ()
dumpGraphViz loc = withGraph loc $ do
    runASTOp $ AST.dumpGraphViz "gui_dump"

typecheck :: GraphLocation -> Empire ()
typecheck loc = withGraph loc $ runTC loc False

-- internal

runTC :: GraphLocation -> Bool -> Command Graph ()
runTC loc flush = do
    g <- get
    Publisher.requestTC loc g flush

printNodeLine :: ASTOp m => NodeId -> m String
printNodeLine nodeId = GraphUtils.getASTPointer nodeId >>= AST.printExpression

withTC :: GraphLocation -> Bool -> Command Graph a -> Empire a
withTC loc flush cmd = withGraph loc $ do
    res <- cmd
    runTC loc flush
    return res

withGraph :: GraphLocation -> Command Graph a -> Empire a
withGraph (GraphLocation pid lid breadcrumb) = withBreadcrumb pid lid breadcrumb

getOutEdges :: ASTOp m => NodeId -> m [InPortRef]
getOutEdges nodeId = do
    graphRep <- GraphBuilder.buildGraph
    let edges    = graphRep ^. APIGraph.connections
        filtered = filter (\(opr, _) -> opr ^. PortRef.srcNodeId == nodeId) edges
    return $ view _2 <$> filtered

disconnectPort :: ASTOp m => InPortRef -> m ()
disconnectPort (InPortRef dstNodeId dstPort) =
    case dstPort of
        Self    -> unAcc dstNodeId
        Arg num -> unApp dstNodeId num

unAcc :: ASTOp m => NodeId -> m ()
unAcc nodeId = do
    dstAst <- GraphUtils.getASTTarget nodeId
    newNodeRef <- AST.removeAccessor dstAst
    GraphUtils.rewireNode nodeId newNodeRef

unApp :: ASTOp m => NodeId -> Int -> m ()
unApp nodeId pos = do
    edges <- GraphBuilder.getEdgePortMapping
    let connectionToOutputEdge = case edges of
            Nothing              -> False
            Just (_, outputEdge) -> outputEdge == nodeId
    if | connectionToOutputEdge -> do
        Just lambda  <- use Graph.insideNode
        astNode <- GraphUtils.getASTTarget lambda
        newNodeRef <- AST.setLambdaOutputToBlank astNode
        GraphUtils.rewireNode lambda newNodeRef
       | otherwise -> do
        astNode <- GraphUtils.getASTTarget nodeId
        newNodeRef <- ASTRemove.removeArg astNode pos
        GraphUtils.rewireNode nodeId newNodeRef

makeAcc :: ASTOp m => NodeId -> NodeId -> Int -> m ()
makeAcc src dst inputPos = do
    edges <- GraphBuilder.getEdgePortMapping
    let connectToInputEdge = case edges of
            Nothing           -> False
            Just (input, _out) -> input == src
    if | connectToInputEdge -> do
        Just lambda  <- use Graph.insideNode
        lambda' <- GraphUtils.getASTTarget lambda
        srcAst  <- AST.getLambdaInputRef lambda' inputPos
        dstAst  <- GraphUtils.getASTTarget dst
        newNodeRef <- AST.makeAccessor srcAst dstAst
        GraphUtils.rewireNode dst newNodeRef
       | otherwise -> do
        srcAst <- GraphUtils.getASTVar    src
        dstAst <- GraphUtils.getASTTarget dst
        newNodeRef <- AST.makeAccessor srcAst dstAst
        GraphUtils.rewireNode dst newNodeRef


makeApp :: ASTOp m => NodeId -> NodeId -> Int -> Int -> m ()
makeApp src dst pos inputPos = do
    edges <- GraphBuilder.getEdgePortMapping
    let (connectToInputEdge, connectToOutputEdge) = case edges of
            Nothing           -> (False, False)
            Just (input, out) -> (input == src, out == dst)
    if | connectToOutputEdge -> do
        Just lambda <- use Graph.insideNode
        srcAst <- GraphUtils.getASTVar    src
        dstAst <- GraphUtils.getASTTarget lambda
        newNodeRef <- AST.redirectLambdaOutput dstAst srcAst
        GraphUtils.rewireNode lambda newNodeRef
       | connectToInputEdge -> do
        Just lambda  <- use Graph.insideNode
        lambda' <- GraphUtils.getASTTarget lambda
        srcAst  <- AST.getLambdaInputRef lambda' inputPos
        dstAst  <- GraphUtils.getASTTarget dst
        newNodeRef <- AST.applyFunction dstAst srcAst pos
        GraphUtils.rewireNode dst newNodeRef
       | otherwise -> do
        srcAst <- GraphUtils.getASTVar    src
        dstAst <- GraphUtils.getASTTarget dst
        newNodeRef <- AST.applyFunction dstAst srcAst pos
        GraphUtils.rewireNode dst newNodeRef

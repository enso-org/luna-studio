{-# LANGUAGE MultiWayIf #-}
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
import           Control.Monad.Except          (throwError)
import           Control.Monad.State           hiding (when)
import           Data.Coerce                   (coerce)
import           Data.IntMap                   (IntMap)
import qualified Data.IntMap                   as IntMap
import           Data.List                     as List (last, sort)
import qualified Data.Map                      as Map
import           Data.Maybe                    (catMaybes)
import           Data.Traversable              (forM)
import qualified Data.UUID                     as UUID
import qualified Data.UUID.V4                  as UUID (nextRandom)
import           Prologue

import           Empire.Data.BreadcrumbHierarchy (addID, addWithLeafs, removeID, topLevelIDs)
import           Empire.Data.Graph               (Graph)
import qualified Empire.Data.Graph               as Graph
import qualified Empire.Data.Library             as Library

import           Empire.API.Data.Breadcrumb      as Breadcrumb (Breadcrumb(..), Named, BreadcrumbItem(..))
import           Empire.API.Data.Connection      (Connection (..))
import           Empire.API.Data.DefaultValue    (PortDefault (Constant), Value (..))
import qualified Empire.API.Data.Graph           as APIGraph
import           Empire.API.Data.GraphLocation   (GraphLocation (..))
import qualified Empire.API.Data.GraphLocation   as GraphLocation
import           Empire.API.Data.Library         (LibraryId)
import           Empire.API.Data.Node            (Node (..), NodeId)
import qualified Empire.API.Data.Node            as Node
import           Empire.API.Data.NodeMeta        (NodeMeta)
import qualified Empire.API.Data.NodeMeta        as NodeMeta
import           Empire.API.Data.Port            (InPort (..), OutPort (..), PortId (..))
import qualified Empire.API.Data.Port            as Port (PortState (..), state)
import           Empire.API.Data.PortRef         (AnyPortRef (..), InPortRef (..), OutPortRef (..))
import qualified Empire.API.Data.PortRef         as PortRef
import           Empire.API.Data.Project         (ProjectId)
import qualified Old.Luna.Syntax.Model.Network.Builder  as Builder
import qualified Data.HMap.Lazy                     as HMap
import           Empire.Data.NodeMarker             (NodeMarker(..), nodeMarkerKey)
import           Old.Data.Prop                      (prop, ( # ))
import Empire.ASTOp (runASTOp)

import           Debug.Trace                     (trace)
import qualified Empire.Commands.AST             as AST
import           Empire.Commands.Breadcrumb      (withBreadcrumb)
import qualified Empire.Commands.GraphBuilder    as GraphBuilder
import qualified Empire.Commands.GraphUtils      as GraphUtils
import           Empire.Commands.Library         (withLibrary)
import qualified Empire.Commands.Publisher       as Publisher
import           Empire.Empire

generateNodeName :: Command Graph String
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
    newNodeName <- generateNodeName
    (parsedRef, refNode) <- zoom Graph.ast $ AST.addNode uuid newNodeName (convert expr)
    parsedIsLambda <- zoom Graph.ast $ AST.isLambda parsedRef
    zoom Graph.ast $ AST.writeMeta refNode meta
    Graph.nodeMapping . at uuid ?= Graph.MatchNode refNode
    node <- GraphBuilder.buildNode uuid
    if parsedIsLambda then do
        lambdaUUID <- liftIO $ UUID.nextRandom
        lambdaOutput <- zoom Graph.ast $ AST.getLambdaOutputRef parsedRef
        outputIsOneOfTheInputs <- zoom Graph.ast $ runASTOp $ lambdaOutput `AST.isLambdaInput` parsedRef
        when (not outputIsOneOfTheInputs) $ Graph.nodeMapping . at lambdaUUID ?= Graph.AnonymousNode lambdaOutput
        Graph.breadcrumbHierarchy %= addWithLeafs (node ^. Node.nodeId)
            (if outputIsOneOfTheInputs then [] else [lambdaUUID])
        zoom Graph.ast $ runASTOp $ Builder.withRef lambdaOutput $ prop Builder.Meta %~ HMap.insert nodeMarkerKey (NodeMarker lambdaUUID)
    else Graph.breadcrumbHierarchy %= addID (node ^. Node.nodeId)
    Publisher.notifyNodeUpdate loc node
    return node

addPersistentNode :: Node -> Command Graph NodeId
addPersistentNode n = case n ^. Node.nodeType of
    Node.ExpressionNode expr -> do
        let newNodeId = n ^. Node.nodeId
        (parsedRef, refNode) <- zoom Graph.ast $ AST.addNode newNodeId (convert $ n ^. Node.name) (convert expr)
        zoom Graph.ast $ AST.writeMeta refNode (n ^. Node.nodeMeta)
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
    forM nodeIds $ \nodeId -> do
        children <- withTC (loc `descendInto` nodeId) False $ do
            uses Graph.breadcrumbHierarchy topLevelIDs
        removeNodes (loc `descendInto` nodeId) children
    withTC loc False $ forM_ nodeIds removeNodeNoTC

removeNodeNoTC :: NodeId -> Command Graph ()
removeNodeNoTC nodeId = do
    astRef <- GraphUtils.getASTPointer nodeId
    obsoleteEdges <- getOutEdges nodeId
    mapM_ disconnectPort obsoleteEdges
    zoom Graph.ast $ AST.removeSubtree astRef
    Graph.nodeMapping %= Map.delete nodeId
    Graph.breadcrumbHierarchy %= removeID nodeId

updateNodeExpression :: GraphLocation -> NodeId -> NodeId -> Text -> Empire (Maybe Node)
updateNodeExpression loc nodeId newNodeId expr = do
    metaMay <- withGraph loc $ do
        ref <- GraphUtils.getASTPointer nodeId
        zoom Graph.ast $ AST.readMeta ref
    forM metaMay $ \meta ->
        withTC loc False $ do
            removeNodeNoTC nodeId
            addNodeNoTC loc newNodeId expr meta

updateNodeMeta :: GraphLocation -> NodeId -> NodeMeta -> Empire ()
updateNodeMeta loc nodeId newMeta = withGraph loc $ do
    ref <- GraphUtils.getASTPointer nodeId
    oldMetaMay <- zoom Graph.ast $ AST.readMeta ref
    doTCMay <- forM oldMetaMay $ \oldMeta ->
        return $ triggerTC oldMeta newMeta
    zoom Graph.ast $ AST.writeMeta ref newMeta
    withJust doTCMay $ \doTC ->
        when doTC $ runTC loc False
    where
        triggerTC :: NodeMeta -> NodeMeta -> Bool
        triggerTC oldMeta newMeta = oldMeta ^. NodeMeta.displayResult /= newMeta ^. NodeMeta.displayResult

connectCondTC :: Bool -> GraphLocation -> OutPortRef -> InPortRef -> Empire ()
connectCondTC doTC loc outPort inPort = withGraph loc $ do
    connectNoTC loc outPort inPort
    when doTC $ runTC loc False

connect :: GraphLocation -> OutPortRef -> InPortRef -> Empire ()
connect loc outPort inPort = withTC loc False $ connectNoTC loc outPort inPort

connectPersistent :: OutPortRef -> InPortRef -> Command Graph ()
connectPersistent (OutPortRef srcNodeId srcPort) (InPortRef dstNodeId dstPort) = do
    let inputPos = case srcPort of
            All            -> 0   -- FIXME: do not equalise All with Projection 0
            Projection int -> int
    case dstPort of
        Self    -> makeAcc srcNodeId dstNodeId inputPos
        Arg num -> makeApp srcNodeId dstNodeId num inputPos

connectNoTC :: GraphLocation -> OutPortRef -> InPortRef -> Command Graph ()
connectNoTC loc outPort@(OutPortRef srcNodeId srcPort) inPort@(InPortRef dstNodeId dstPort) = connectPersistent outPort inPort

setDefaultValue :: GraphLocation -> AnyPortRef -> PortDefault -> Empire ()
setDefaultValue loc portRef val = withTC loc False $ setDefaultValue' portRef val

setDefaultValue' :: AnyPortRef -> PortDefault -> Command Graph ()
setDefaultValue' portRef val = do
    parsed <- zoom Graph.ast $ AST.addDefault val
    (nodeId, newRef) <- case portRef of
        InPortRef' (InPortRef nodeId port) -> do
            ref <- GraphUtils.getASTTarget nodeId
            newRef <- zoom Graph.ast $ case port of
                Self    -> AST.makeAccessor parsed ref
                Arg num -> AST.applyFunction ref parsed num
            return (nodeId, newRef)
        OutPortRef' (OutPortRef nodeId _) -> return (nodeId, parsed)
    GraphUtils.rewireNode nodeId newRef

disconnect :: GraphLocation -> InPortRef -> Empire ()
disconnect loc port@(InPortRef dstNodeId dstPort) = withTC loc False $ disconnectPort port

getNodeMeta :: GraphLocation -> NodeId -> Empire (Maybe NodeMeta)
getNodeMeta loc nodeId = withGraph loc $ do
    ref <- GraphUtils.getASTPointer nodeId
    zoom Graph.ast $ AST.readMeta ref

getCode :: GraphLocation -> Empire String
getCode loc = withGraph loc $ do
    inFunction <- use Graph.insideNode
    function <- forM inFunction $ \nodeId -> do
        ptr <- GraphUtils.getASTPointer nodeId
        header <- zoom Graph.ast $ AST.printFunctionHeader ptr
        lam <- GraphUtils.getASTTarget nodeId
        ret <- zoom Graph.ast $ AST.printReturnValue lam
        return (header, ret)
    returnedNodeId <- GraphBuilder.nodeConnectedToOutput
    allNodes <- uses Graph.breadcrumbHierarchy topLevelIDs
    refs     <- mapM GraphUtils.getASTPointer $ flip filter allNodes $ \nid ->
        case returnedNodeId of
            Just id -> id /= nid
            _       -> True
    metas    <- zoom Graph.ast $ mapM AST.readMeta refs
    let sorted = fmap snd $ sort $ zip metas allNodes
    lines <- mapM printNodeLine sorted
    return $ unlines $ case function of
        Just (header, ret) -> header : map ("    " ++) (lines ++ [ret])
        _                  -> lines

getGraph :: GraphLocation -> Empire APIGraph.Graph
getGraph loc = withTC loc True GraphBuilder.buildGraph

getNodes :: GraphLocation -> Empire [Node]
getNodes loc = withTC loc True $ view APIGraph.nodes <$> GraphBuilder.buildGraph

getConnections :: GraphLocation -> Empire [(OutPortRef, InPortRef)]
getConnections loc = withTC loc True $ view APIGraph.connections <$> GraphBuilder.buildGraph

decodeLocation :: GraphLocation -> Empire (Breadcrumb (Named BreadcrumbItem))
decodeLocation loc@(GraphLocation _ _ crumbs) = withGraph loc $ GraphBuilder.decodeBreadcrumbs crumbs

renameNode :: GraphLocation -> NodeId -> Text -> Empire ()
renameNode loc nid name = withTC loc False $ do
    vref <- GraphUtils.getASTVar nid
    zoom Graph.ast $ AST.renameVar vref (convert name)

dumpGraphViz :: GraphLocation -> Empire ()
dumpGraphViz loc = withGraph loc $ do
    zoom Graph.ast $ AST.dumpGraphViz "gui_dump"

typecheck :: GraphLocation -> Empire ()
typecheck loc = withGraph loc $ runTC loc False

-- internal

runTC :: GraphLocation -> Bool -> Command Graph ()
runTC loc flush = do
    g <- get
    Publisher.requestTC loc g flush

printNodeLine :: NodeId -> Command Graph String
printNodeLine nodeId = GraphUtils.getASTPointer nodeId >>= (zoom Graph.ast . AST.printExpression)

withTC :: GraphLocation -> Bool -> Command Graph a -> Empire a
withTC loc flush cmd = withGraph loc $ do
    res <- cmd
    runTC loc flush
    return res

withGraph :: GraphLocation -> Command Graph a -> Empire a
withGraph (GraphLocation pid lid breadcrumb) = withBreadcrumb pid lid breadcrumb

getOutEdges :: NodeId -> Command Graph [InPortRef]
getOutEdges nodeId = do
    graphRep <- GraphBuilder.buildGraph
    let edges    = graphRep ^. APIGraph.connections
        filtered = filter (\(opr, _) -> opr ^. PortRef.srcNodeId == nodeId) edges
    return $ view _2 <$> filtered

disconnectPort :: InPortRef -> Command Graph ()
disconnectPort (InPortRef dstNodeId dstPort) =
    case dstPort of
        Self    -> unAcc dstNodeId
        Arg num -> unApp dstNodeId num

unAcc :: NodeId -> Command Graph ()
unAcc nodeId = do
    dstAst <- GraphUtils.getASTTarget nodeId
    newNodeRef <- zoom Graph.ast $ AST.removeAccessor dstAst
    GraphUtils.rewireNode nodeId newNodeRef

unApp :: NodeId -> Int -> Command Graph ()
unApp nodeId pos = do
    edges <- GraphBuilder.getEdgePortMapping
    let connectionToOutputEdge = case edges of
            Nothing              -> False
            Just (_, outputEdge) -> outputEdge == nodeId
    if | connectionToOutputEdge -> do
        lambda  <- use Graph.insideNode <?!> "impossible: removing connection to output edge while outside node"
        astNode <- GraphUtils.getASTTarget lambda
        newNodeRef <- zoom Graph.ast $ AST.setLambdaOutputToBlank astNode
        GraphUtils.rewireNode lambda newNodeRef
       | otherwise -> do
        astNode <- GraphUtils.getASTTarget nodeId
        newNodeRef <- zoom Graph.ast $ AST.unapplyArgument astNode pos
        GraphUtils.rewireNode nodeId newNodeRef

makeAcc :: NodeId -> NodeId -> Int -> Command Graph ()
makeAcc src dst inputPos = do
    edges <- GraphBuilder.getEdgePortMapping
    let (connectToInputEdge, connectToOutputEdge) = case edges of
            Nothing           -> (False, False)
            Just (input, out) -> (input == src, out == dst)
    if | connectToInputEdge -> do
        lambda  <- use Graph.insideNode <?!> "impossible: connecting to input edge while outside node"
        lambda' <- GraphUtils.getASTTarget lambda
        srcAst  <- zoom Graph.ast $ AST.getLambdaInputRef lambda' inputPos
        dstAst  <- GraphUtils.getASTTarget dst
        newNodeRef <- zoom Graph.ast $ AST.makeAccessor srcAst dstAst
        GraphUtils.rewireNode dst newNodeRef
       | otherwise -> do
        srcAst <- GraphUtils.getASTVar    src
        dstAst <- GraphUtils.getASTTarget dst
        newNodeRef <- zoom Graph.ast $ AST.makeAccessor srcAst dstAst
        GraphUtils.rewireNode dst newNodeRef


makeApp :: NodeId -> NodeId -> Int -> Int -> Command Graph ()
makeApp src dst pos inputPos = do
    edges <- GraphBuilder.getEdgePortMapping
    let (connectToInputEdge, connectToOutputEdge) = case edges of
            Nothing           -> (False, False)
            Just (input, out) -> (input == src, out == dst)
    if | connectToOutputEdge -> do
        lambda <- use Graph.insideNode <?!> "impossible: connecting to output edge while outside node"
        srcAst <- GraphUtils.getASTVar    src
        dstAst <- GraphUtils.getASTTarget lambda
        newNodeRef <- zoom Graph.ast $ AST.redirectLambdaOutput dstAst srcAst
        GraphUtils.rewireNode lambda newNodeRef
       | connectToInputEdge -> do
        lambda  <- use Graph.insideNode <?!> "impossible: connecting to input edge while outside node"
        lambda' <- GraphUtils.getASTTarget lambda
        srcAst  <- zoom Graph.ast $ AST.getLambdaInputRef lambda' inputPos
        dstAst  <- GraphUtils.getASTTarget dst
        newNodeRef <- zoom Graph.ast $ AST.applyFunction dstAst srcAst pos
        GraphUtils.rewireNode dst newNodeRef
       | otherwise -> do
        srcAst <- GraphUtils.getASTVar    src
        dstAst <- GraphUtils.getASTTarget dst
        newNodeRef <- zoom Graph.ast $ AST.applyFunction dstAst srcAst pos
        GraphUtils.rewireNode dst newNodeRef

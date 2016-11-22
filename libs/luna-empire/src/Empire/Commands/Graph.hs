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
    , setDefaultValue
    , renameNode
    , dumpGraphViz
    , typecheck
    ) where

import           Control.Monad                   (forM, forM_)
import           Control.Monad.Except            (throwError)
import           Control.Monad.State             hiding (when)
import           Data.IntMap                     (IntMap)
import qualified Data.IntMap                     as IntMap
import           Data.List                       (sort)
import qualified Data.Map                        as Map
import           Data.Maybe                      (catMaybes)
import           Data.Text.Lazy                  (Text)
import qualified Data.Text.Lazy                  as Text
import           Data.Traversable                (forM)
import qualified Data.UUID                       as UUID
import           Prologue

import           Empire.Data.BreadcrumbHierarchy (addID, removeID)
import           Empire.Data.Graph               (Graph)
import qualified Empire.Data.Graph               as Graph
import qualified Empire.Data.Library             as Library

import           Empire.API.Data.Breadcrumb      (Breadcrumb, Named, BreadcrumbItem)
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
    refNode <- zoom Graph.ast $ AST.addNode uuid newNodeName (Text.unpack expr)
    zoom Graph.ast $ AST.writeMeta refNode meta
    Graph.nodeMapping . at uuid ?= refNode
    node <- GraphBuilder.buildNode uuid
    Graph.breadcrumbHierarchy %= addID (node ^. Node.nodeId)
    Publisher.notifyNodesUpdate loc node
    return node

addPersistentNode :: Node -> Command Graph NodeId
addPersistentNode n = case n ^. Node.nodeType of
    Node.ExpressionNode expr -> do
        let newNodeId = n ^. Node.nodeId
        refNode <- zoom Graph.ast $ AST.addNode newNodeId (Text.unpack $ n ^. Node.name) (Text.unpack expr)
        zoom Graph.ast $ AST.writeMeta refNode (n ^. Node.nodeMeta)
        Graph.nodeMapping . at newNodeId ?= refNode
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


removeNodes :: GraphLocation -> [NodeId] -> Empire ()
removeNodes loc nodeIds = withTC loc False $ forM_ nodeIds removeNodeNoTC

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
connectPersistent (OutPortRef srcNodeId All) (InPortRef dstNodeId dstPort) =
    case dstPort of
        Self    -> makeAcc srcNodeId dstNodeId
        Arg num -> makeApp srcNodeId dstNodeId num
connectPersistent _ _ = throwError "Source port should be All"

connectNoTC :: GraphLocation -> OutPortRef -> InPortRef -> Command Graph ()
connectNoTC loc outPort@(OutPortRef srcNodeId All) inPort@(InPortRef dstNodeId dstPort) = connectPersistent outPort inPort

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
    allNodes <- uses Graph.nodeMapping Map.keys
    refs     <- mapM GraphUtils.getASTPointer allNodes
    metas    <- zoom Graph.ast $ mapM AST.readMeta refs
    let sorted = fmap snd $ sort $ zip metas allNodes
    lines <- sequence $ printNodeLine <$> sorted
    return $ unlines lines

getGraph :: GraphLocation -> Empire APIGraph.Graph
getGraph loc = withTC loc True GraphBuilder.buildGraph

decodeLocation :: GraphLocation -> Empire (Breadcrumb (Named BreadcrumbItem))
decodeLocation loc@(GraphLocation _ _ crumbs) = withGraph loc $ GraphBuilder.decodeBreadcrumbs crumbs

renameNode :: GraphLocation -> NodeId -> Text -> Empire ()
renameNode loc nid name = withTC loc False $ do
    vref <- GraphUtils.getASTVar nid
    zoom Graph.ast $ AST.renameVar vref (Text.unpack name)

dumpGraphViz :: GraphLocation -> Empire ()
dumpGraphViz loc = withGraph loc $ do
    zoom Graph.ast   $ AST.dumpGraphViz "gui_dump"
    {-zoom Graph.tcAST $ AST.dumpGraphViz "gui_tc_dump"-}

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
    astNode <- GraphUtils.getASTTarget nodeId
    newNodeRef <- zoom Graph.ast $ AST.unapplyArgument astNode pos
    GraphUtils.rewireNode nodeId newNodeRef

makeAcc :: NodeId -> NodeId -> Command Graph ()
makeAcc src dst = do
    srcAst <- GraphUtils.getASTVar    src
    dstAst <- GraphUtils.getASTTarget dst
    newNodeRef <- zoom Graph.ast $ AST.makeAccessor srcAst dstAst
    GraphUtils.rewireNode dst newNodeRef

makeApp :: NodeId -> NodeId -> Int -> Command Graph ()
makeApp src dst pos = do
    srcAst <- GraphUtils.getASTVar    src
    dstAst <- GraphUtils.getASTTarget dst
    newNodeRef <- zoom Graph.ast $ AST.applyFunction dstAst srcAst pos
    GraphUtils.rewireNode dst newNodeRef

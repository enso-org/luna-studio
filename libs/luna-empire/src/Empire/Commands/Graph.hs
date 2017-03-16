{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TupleSections       #-}

module Empire.Commands.Graph
    ( addNode
    , addNodeCondTC
    , addPort
    , addSubgraph
    , removeNodes
    , movePort
    , removePort
    , renamePort
    , setNodeExpression
    , setNodeMeta
    , updatePort
    , connect
    , connectPersistent
    , connectCondTC
    , connectNoTC
    , decodeLocation
    , disconnect
    , getNodeMeta
    , getNodeIdSequence
    , getCode
    , getGraph
    , getNodes
    , getConnections
    , setPortDefault
    , renameNode
    , dumpGraphViz
    , typecheck
    , withTC
    , withGraph
    ) where

import           Control.Monad                 (forM, forM_)
import           Control.Monad.Catch           (MonadCatch(..))
import           Control.Monad.State           hiding (when)
import           Data.Coerce                   (coerce)
import           Data.List                     (sort)
import qualified Data.Map                      as Map
import           Data.Maybe                    (catMaybes)
import           Data.Foldable                 (toList)
import           Data.Text                     (Text)
import qualified Data.Text                     as Text
import qualified Data.UUID                     as UUID
import qualified Data.UUID.V4                  as UUID (nextRandom)
import           Empire.Prelude

import           Empire.Data.AST                 (NodeRef, NotInputEdgeException (..), NotUnifyException)
import qualified Empire.Data.BreadcrumbHierarchy as BH
import           Empire.Data.Graph               (Graph)
import qualified Empire.Data.Graph               as Graph
import           Empire.Data.Layers              (Marker, NodeMarker(..))

import           Empire.API.Data.Breadcrumb      as Breadcrumb (Breadcrumb(..), Named, BreadcrumbItem(..))
import qualified Empire.API.Data.Connection      as Connection
import           Empire.API.Data.Connection      (Connection (..))
import           Empire.API.Data.PortDefault     (PortDefault (Constant))
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
import           Empire.ASTOp                    (ASTOp, runASTOp, runAliasAnalysis)

import qualified Empire.ASTOps.Builder           as ASTBuilder
import qualified Empire.ASTOps.Modify            as ASTModify
import qualified Empire.ASTOps.Read              as ASTRead
import qualified Empire.ASTOps.Parse             as ASTParse
import qualified Empire.ASTOps.Print             as ASTPrint
import qualified Empire.ASTOps.Remove            as ASTRemove
import qualified Empire.Commands.AST             as AST
import           Empire.Commands.Breadcrumb      (withBreadcrumb)
import qualified Empire.Commands.GraphBuilder    as GraphBuilder
import qualified Empire.Commands.GraphUtils      as GraphUtils
import qualified Empire.Commands.Publisher       as Publisher
import           Empire.Empire

import qualified Luna.IR            as IR
import qualified OCI.IR.Combinators as IR (changeSource, deleteSubtree, replaceNode)

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
    ((parsedRef, refNode), nodeItem) <- runASTOp $ do
        newNodeName <- generateNodeName
        parsedNode <- AST.addNode uuid newNodeName (Text.unpack expr)
        let nodeItem = BH.BItem Map.empty Nothing (Just (uuid, BH.MatchNode $ parsedNode ^. _2)) Nothing
        Graph.breadcrumbHierarchy . BH.children . at uuid ?= nodeItem
        return (parsedNode, nodeItem)
    runAliasAnalysis
    node <- runASTOp $ do
        AST.writeMeta refNode meta
        parsedIsLambda <- ASTRead.isLambda parsedRef
        node <- GraphBuilder.buildNode uuid
        when parsedIsLambda $ do
            lambdaUUID             <- liftIO $ UUID.nextRandom
            lambdaOutput           <- ASTRead.getLambdaOutputRef parsedRef
            outputIsOneOfTheInputs <- AST.isTrivialLambda parsedRef
            let anonOutput = if   not outputIsOneOfTheInputs
                             then Just $ BH.BItem Map.empty Nothing (Just (lambdaUUID, BH.AnonymousNode lambdaOutput)) Nothing
                             else Nothing
                lamItem    = nodeItem & BH.children . at lambdaUUID .~ anonOutput
                                      & BH.body ?~ lambdaOutput
            Graph.breadcrumbHierarchy . BH.children . at uuid  ?= lamItem
            IR.writeLayer @Marker (Just $ NodeMarker lambdaUUID) lambdaOutput
        updateNodeSequence
        return node
    Publisher.notifyNodeUpdate loc node
    return node

updateNodeSequenceWithOutput :: ASTOp m => Maybe NodeRef -> m ()
updateNodeSequenceWithOutput outputRef = do
    newSeq     <- makeCurrentSeq
    updateGraphSeq newSeq outputRef

updateNodeSequence :: ASTOp m => m ()
updateNodeSequence = do
    currentTgt <- ASTRead.getCurrentASTTarget
    outputRef  <- mapM ASTRead.getLambdaOutputRef    currentTgt
    updateNodeSequenceWithOutput outputRef

makeCurrentSeq :: ASTOp m => m (Maybe NodeRef)
makeCurrentSeq = do
  allNodes   <- uses Graph.breadcrumbHierarchy BH.topLevelIDs
  sortedRefs <- AST.sortByPosition allNodes
  AST.makeSeq sortedRefs

updateGraphSeq :: ASTOp m => Maybe NodeRef -> Maybe NodeRef -> m ()
updateGraphSeq newSeq outputRef = do
    oldSeq <- use $ Graph.breadcrumbHierarchy . BH.body
    currentTgt <- ASTRead.getCurrentASTTarget
    outLink    <- mapM ASTRead.getFirstNonLambdaLink currentTgt
    newOut     <- case (outputRef, newSeq) of
        (Just ref, Just new) -> Just . IR.generalize <$> IR.seq new ref
        (Just ref, Nothing)  -> return $ Just ref
        (Nothing,  Just n)   -> return $ Just n
        _                    -> return Nothing
    case (,) <$> outLink <*> newOut of
        Just (l, o) -> IR.changeSource l o
        Nothing     -> return ()
    when (newOut /= oldSeq) $ mapM_ ASTRemove.removeSubtree oldSeq
    Graph.breadcrumbHierarchy . BH.body .= newOut

-- TODO[MK]: Figure out the logic of seqing and migrate to a uniform solution
getNodeIdSequence :: GraphLocation -> Empire [NodeId]
getNodeIdSequence loc = withGraph loc $ runASTOp $ do
    lref <- ASTRead.getCurrentASTTarget
    nodeSeq <- do
        bodySeq <- case lref of
            Just l -> ASTRead.getLambdaBodyRef l
            _      -> use $ Graph.breadcrumbHierarchy . BH.body
        case bodySeq of
            Just b -> AST.readSeq b
            _      -> return []
    catMaybes <$> mapM nodeIdInsideLambda nodeSeq

nodeIdInsideLambda :: ASTOp m => NodeRef -> m (Maybe NodeId)
nodeIdInsideLambda node = (ASTRead.getVarNode node >>= ASTRead.getNodeId) `catch`
    (\(_e :: NotUnifyException) -> return Nothing)

--TODO[MM]: Replace NodeId with AnyPortRef and allow to add at any position
addPort :: GraphLocation -> NodeId -> Empire Node
addPort loc nid = withGraph loc $ runASTOp $ do
    Just ref <- ASTRead.getCurrentASTTarget
    edges <- GraphBuilder.getEdgePortMapping
    when ((fst <$> edges) /= Just nid) $ throwM NotInputEdgeException
    ASTModify.addLambdaArg ref
    inputEdge <- GraphBuilder.buildConnections >>= \c -> GraphBuilder.buildInputEdge c nid
    return inputEdge

generateNodeId :: IO NodeId
generateNodeId = UUID.nextRandom

addSubgraph :: GraphLocation -> [Node] -> [Connection] -> Empire ([Node])
addSubgraph loc nodes conns = withTC loc False $ do
    newNodes <- fmap catMaybes $ forM nodes $ \n -> case n ^. Node.nodeType of
        Node.ExpressionNode expr -> Just <$> addNodeNoTC loc (n ^. Node.nodeId) expr (n ^. Node.nodeMeta)
        _ -> return Nothing
    forM_ conns $ \(Connection src dst) -> connectNoTC loc src dst
    return newNodes

descendInto :: GraphLocation -> NodeId -> GraphLocation
descendInto (GraphLocation pid lid breadcrumb) nid = GraphLocation pid lid breadcrumb'
    where
        breadcrumb' = coerce $ coerce breadcrumb ++ [Breadcrumb.Lambda nid]

removeNodes :: GraphLocation -> [NodeId] -> Empire ()
removeNodes loc nodeIds = withTC loc False $ runASTOp $ do
    forM_ nodeIds removeNodeNoTC
    when (not . null $ nodeIds) $ updateNodeSequence

removeNodeNoTC :: ASTOp m => NodeId -> m ()
removeNodeNoTC nodeId = do
    astRef        <- GraphUtils.getASTPointer nodeId
    obsoleteEdges <- getOutEdges nodeId
    mapM_ disconnectPort obsoleteEdges
    Graph.breadcrumbHierarchy . BH.children . at nodeId .= Nothing

removePort :: GraphLocation -> AnyPortRef -> Empire Node
removePort loc portRef = withGraph loc $ runASTOp $ do
    let nodeId = portRef ^. PortRef.nodeId
    Just lambda <- preuse $ Graph.breadcrumbHierarchy . BH.self . _Just . _1
    Just ref    <- ASTRead.getCurrentASTTarget
    edges <- GraphBuilder.getEdgePortMapping
    newRef <- case edges of
        Just (input, _output) -> do
            if nodeId == input then ASTModify.removeLambdaArg ref $ portRef ^. PortRef.portId
                               else throwM NotInputEdgeException
        _ -> return ref
    when (ref /= newRef) $ GraphUtils.rewireNode lambda newRef
    GraphBuilder.buildConnections >>= \c -> GraphBuilder.buildInputEdge c nodeId

movePort :: GraphLocation -> AnyPortRef -> Int -> Empire Node
movePort loc portRef newPosition = withGraph loc $ runASTOp $ do
    let nodeId = portRef ^. PortRef.nodeId
    Just ref    <- ASTRead.getCurrentASTTarget
    edges       <- GraphBuilder.getEdgePortMapping
    newRef      <- case edges of
        Just (input, _) -> do
            if nodeId == input then ASTModify.moveLambdaArg ref (portRef ^. PortRef.portId) newPosition
                               else throwM NotInputEdgeException
        _ -> throwM NotInputEdgeException
    when (ref /= newRef) $ ASTModify.rewireCurrentNode newRef
    GraphBuilder.buildConnections >>= \c -> GraphBuilder.buildInputEdge c nodeId

renamePort :: GraphLocation -> AnyPortRef -> String -> Empire Node
renamePort loc portRef newName = withGraph loc $ runASTOp $ do
    let nodeId = portRef ^. PortRef.nodeId
    Just ref    <- ASTRead.getCurrentASTTarget
    edges       <- GraphBuilder.getEdgePortMapping
    _newRef     <- case edges of
        Just (input, _) -> do
            if nodeId == input then ASTModify.renameLambdaArg ref (portRef ^. PortRef.portId) newName
                               else throwM NotInputEdgeException
        _ -> throwM NotInputEdgeException
    GraphBuilder.buildConnections >>= \c -> GraphBuilder.buildInputEdge c nodeId

setNodeExpression :: GraphLocation -> NodeId -> Text -> Empire Node
setNodeExpression loc nodeId expr = withTC loc False $ do
    parsedExpr <- runASTOp $ do
        (exprName, parsedExpr) <- ASTParse.parseExpr $ Text.unpack expr
        target                 <- ASTRead.getASTTarget nodeId
        IR.replaceNode target parsedExpr
        IR.deleteSubtree target
        forM_ exprName $ \newName -> renameNodeGraph nodeId newName
        return parsedExpr
    runAliasAnalysis
    runASTOp $ do
        parsedIsLambda <- ASTRead.isLambda parsedExpr
        when parsedIsLambda $ do
            lambdaUUID             <- liftIO $ UUID.nextRandom
            lambdaOutput           <- ASTRead.getLambdaOutputRef parsedExpr
            outputIsOneOfTheInputs <- AST.isTrivialLambda        parsedExpr
            Just nodeItem          <- use $ Graph.breadcrumbHierarchy . BH.children . at nodeId
            let anonOutput = if   not outputIsOneOfTheInputs
                             then Just $ BH.BItem Map.empty Nothing (Just (lambdaUUID, BH.AnonymousNode lambdaOutput)) Nothing
                             else Nothing
                lamItem    = nodeItem & BH.children . at lambdaUUID .~ anonOutput
                                      & BH.body ?~ lambdaOutput
            Graph.breadcrumbHierarchy . BH.children . at nodeId  ?= lamItem
            IR.writeLayer @Marker (Just $ NodeMarker lambdaUUID) lambdaOutput
        updateNodeSequence
    node <- runASTOp $ GraphBuilder.buildNode nodeId
    Publisher.notifyNodeUpdate loc node
    return node

setNodeMeta :: GraphLocation -> NodeId -> NodeMeta -> Empire ()
setNodeMeta loc nodeId newMeta = withGraph loc $ do
    doTCMay <- runASTOp $ do
        ref <- GraphUtils.getASTPointer nodeId
        oldMetaMay <- AST.readMeta ref
        doTCMay <- forM oldMetaMay $ \oldMeta ->
            return $ triggerTC oldMeta newMeta
        AST.writeMeta ref newMeta
        updateNodeSequence
        return doTCMay
    forM_ doTCMay $ \doTC ->
        when doTC $ runTC loc False
    where
        triggerTC :: NodeMeta -> NodeMeta -> Bool
        triggerTC oldMeta' newMeta' = oldMeta' ^. NodeMeta.displayResult /= newMeta' ^. NodeMeta.displayResult

updatePort :: GraphLocation -> AnyPortRef -> Either Int String -> Empire AnyPortRef
updatePort = $notImplemented

connectCondTC :: Bool -> GraphLocation -> OutPortRef -> InPortRef -> Empire Connection
connectCondTC doTC loc outPort inPort = withGraph loc $ do
    result <- connectNoTC loc outPort inPort
    when doTC $ runTC loc False
    return result

connect :: GraphLocation -> OutPortRef -> InPortRef -> Empire Connection
connect loc outPort inPort = withTC loc False $ connectNoTC loc outPort inPort

connectPersistent :: ASTOp m => OutPortRef -> InPortRef -> m Connection
connectPersistent src@(OutPortRef srcNodeId srcPort) dst@(InPortRef dstNodeId dstPort) = do
    let inputPos = case srcPort of
            All            -> 0   -- FIXME: do not equalise All with Projection 0
            Projection int -> int
    case dstPort of
        Self    -> makeAcc srcNodeId dstNodeId inputPos
        Arg num -> makeApp srcNodeId dstNodeId num inputPos
    return $ Connection src dst

connectNoTC :: GraphLocation -> OutPortRef -> InPortRef -> Command Graph Connection
connectNoTC loc outPort inPort@(InPortRef nid _) = do
    (connection, nodeToUpdate) <- runASTOp $ do
        connection <- connectPersistent outPort inPort

        -- if input port is not an edge, send update to gui
        edges <- GraphBuilder.getEdgePortMapping
        let nodeToUpdate = case edges of
                Just (input, output) -> do
                    if (nid /= input && nid /= output) then Just <$> GraphBuilder.buildNode nid
                                                       else return Nothing
                _ -> Just <$> GraphBuilder.buildNode nid
        (connection,) <$> nodeToUpdate
    forM_ nodeToUpdate $ \n -> do
        Publisher.notifyNodeUpdate loc n
    return connection

setPortDefault :: GraphLocation -> AnyPortRef -> PortDefault -> Empire ()
setPortDefault loc portRef val = withTC loc False $ runASTOp $ setPortDefault' portRef val

setPortDefault' :: ASTOp m => AnyPortRef -> PortDefault -> m ()
setPortDefault' portRef val = do
    parsed <- ASTParse.parsePortDefault val
    (nodeId, newRef) <- case portRef of
        InPortRef' (InPortRef nodeId port) -> do
            ref <- GraphUtils.getASTTarget nodeId
            newRef <- case port of
                Self    -> ASTBuilder.makeAccessor parsed ref
                Arg num -> ASTBuilder.applyFunction ref parsed num
            return (nodeId, newRef)
        OutPortRef' (OutPortRef nodeId _) -> return (nodeId, parsed)
    GraphUtils.rewireNode nodeId newRef

disconnect :: GraphLocation -> InPortRef -> Empire ()
disconnect loc port@(InPortRef nid _) = withTC loc False $ do
    nodeToUpdate <- runASTOp $ do
        disconnectPort port

        -- if input port is not an edge, send update to gui
        edges <- GraphBuilder.getEdgePortMapping
        case edges of
            Just (input, output) -> do
                if (nid /= input && nid /= output) then Just <$> GraphBuilder.buildNode nid
                                                   else return Nothing
            _ -> Just <$> GraphBuilder.buildNode nid
    forM_ nodeToUpdate $ Publisher.notifyNodeUpdate loc
    return ()

getNodeMeta :: GraphLocation -> NodeId -> Empire (Maybe NodeMeta)
getNodeMeta loc nodeId = withGraph loc $ runASTOp $ do
    ref <- GraphUtils.getASTPointer nodeId
    AST.readMeta ref

getCode :: GraphLocation -> Empire String
getCode loc = withGraph loc $ runASTOp $ do
    function <- ASTPrint.printCurrentFunction
    returnedNodeId <- GraphBuilder.nodeConnectedToOutput
    allNodes <- uses Graph.breadcrumbHierarchy BH.topLevelIDs
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
getGraph loc = withTC loc True $ runASTOp (AST.dumpGraphViz "snap") >> runASTOp GraphBuilder.buildGraph

getNodes :: GraphLocation -> Empire [Node]
getNodes loc = withTC loc True $ runASTOp $ view APIGraph.nodes <$> GraphBuilder.buildGraph

getConnections :: GraphLocation -> Empire [(OutPortRef, InPortRef)]
getConnections loc = withTC loc True $ runASTOp $ view APIGraph.connections <$> GraphBuilder.buildGraph

decodeLocation :: GraphLocation -> Empire (Breadcrumb (Named BreadcrumbItem))
decodeLocation loc@(GraphLocation _ _ crumbs) = withGraph loc $ GraphBuilder.decodeBreadcrumbs crumbs

renameNode :: GraphLocation -> NodeId -> Text -> Empire ()
renameNode loc nid name = withTC loc False $ do
    runASTOp $ renameNodeGraph nid name
    runAliasAnalysis

renameNodeGraph :: ASTOp m => NodeId -> Text -> m ()
renameNodeGraph nid name = do
    vref <- GraphUtils.getASTVar nid
    ASTModify.renameVar vref (Text.unpack name)

dumpGraphViz :: GraphLocation -> Empire ()
dumpGraphViz loc = withGraph loc $ return ()

typecheck :: GraphLocation -> Empire ()
typecheck loc = withGraph loc $ runTC loc False

-- internal

runTC :: GraphLocation -> Bool -> Command Graph ()
runTC loc flush = do
    g <- get
    Publisher.requestTC loc g flush

printNodeLine :: ASTOp m => NodeId -> m String
printNodeLine nodeId = GraphUtils.getASTPointer nodeId >>= ASTPrint.printExpression

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
    newNodeRef <- ASTBuilder.removeAccessor dstAst
    GraphUtils.rewireNode nodeId newNodeRef

unApp :: ASTOp m => NodeId -> Int -> m ()
unApp nodeId pos = do
    edges <- GraphBuilder.getEdgePortMapping
    let connectionToOutputEdge = case edges of
            Nothing              -> False
            Just (_, outputEdge) -> outputEdge == nodeId
    if | connectionToOutputEdge -> do
        Just astNode <- ASTRead.getCurrentASTTarget
        newNodeRef   <- ASTModify.setLambdaOutputToBlank astNode
        ASTModify.rewireCurrentNode newNodeRef
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
        Just lambda' <- ASTRead.getCurrentASTTarget
        srcAst  <- AST.getLambdaInputRef lambda' inputPos
        dstAst  <- GraphUtils.getASTTarget dst
        newNodeRef <- ASTBuilder.makeAccessor srcAst dstAst
        GraphUtils.rewireNode dst newNodeRef
       | otherwise -> do
        srcAst <- GraphUtils.getASTVar    src
        dstAst <- GraphUtils.getASTTarget dst
        newNodeRef <- ASTBuilder.makeAccessor srcAst dstAst
        GraphUtils.rewireNode dst newNodeRef


makeApp :: ASTOp m => NodeId -> NodeId -> Int -> Int -> m ()
makeApp src dst pos inputPos = do
    edges <- GraphBuilder.getEdgePortMapping
    let (connectToInputEdge, connectToOutputEdge) = case edges of
            Nothing           -> (False, False)
            Just (input, out) -> (input == src, out == dst)
    case (connectToInputEdge, connectToOutputEdge) of
        (True, True) -> do
            Just lambda' <- ASTRead.getCurrentASTTarget
            srcAst <- AST.getLambdaInputRef lambda' inputPos
            updateNodeSequenceWithOutput $ Just srcAst
        (False, True) -> do
            srcAst     <- GraphUtils.getASTVar    src
            updateNodeSequenceWithOutput $ Just srcAst
        (True, False) -> do
            Just lambda' <- ASTRead.getCurrentASTTarget
            srcAst  <- AST.getLambdaInputRef lambda' inputPos
            dstAst  <- GraphUtils.getASTTarget dst
            newNodeRef <- ASTBuilder.applyFunction dstAst srcAst pos
            GraphUtils.rewireNode dst newNodeRef
        (False, False) -> do
            srcAst <- GraphUtils.getASTVar    src
            dstAst <- GraphUtils.getASTTarget dst
            newNodeRef <- ASTBuilder.applyFunction dstAst srcAst pos
            GraphUtils.rewireNode dst newNodeRef

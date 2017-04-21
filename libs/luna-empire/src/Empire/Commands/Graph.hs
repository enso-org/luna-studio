{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TupleSections       #-}

module Empire.Commands.Graph
    ( addNode
    , addNodeCondTC
    , addPort
    , addPortWithConnection
    , addSubgraph
    , removeNodes
    , movePort
    , removePort
    , renamePort
    , setNodeExpression
    , setNodeMeta
    , connect
    , connectPersistent
    , connectCondTC
    , connectNoTC
    , decodeLocation
    , disconnect
    , getNodeMeta
    , getBuffer
    , getCode
    , getGraph
    , getNodes
    , getConnections
    , setPortDefault
    , getPortDefault
    , renameNode
    , dumpGraphViz
    , openFile
    , typecheck
    , substituteCode
    , loadCode
    , withTC
    , withGraph
    ) where

import           Control.Monad                 (forM, forM_)
import           Control.Monad.Catch           (MonadCatch(..), handle)
import           Control.Monad.State           hiding (when)
import           Control.Arrow                 ((&&&))
import           Control.Monad.Error           (throwError)
import           Data.Coerce                   (coerce)
import           Data.List                     (sort, group)
import qualified Data.Map                      as Map
import           Data.Maybe                    (catMaybes, fromMaybe, isJust)
import           Data.Foldable                 (toList)
import           Data.Text                     (Text)
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text
import qualified Data.UUID                     as UUID
import qualified Data.UUID.V4                  as UUID (nextRandom)
import           Empire.Prelude                hiding (toList)
import qualified Safe
import qualified System.IO                     as IO

import           Empire.Data.AST                 (NodeRef, NotInputEdgeException (..), NotUnifyException,
                                                  InvalidConnectionException (..),
                                                  astExceptionFromException, astExceptionToException)
import qualified Empire.Data.BreadcrumbHierarchy as BH
import           Empire.Data.Graph               (Graph)
import qualified Empire.Data.Graph               as Graph
import qualified Empire.Data.Library             as Library
import           Empire.Data.Layers              (CodeMarkers, Marker)

import           Empire.API.Data.Breadcrumb      (Breadcrumb (..), Named, BreadcrumbItem)
import qualified Empire.API.Data.Breadcrumb      as Breadcrumb
import qualified Empire.API.Data.Connection      as Connection
import           Empire.API.Data.Connection      (Connection (..))
import           Empire.API.Data.PortDefault     (PortDefault (Constant))
import qualified Empire.API.Data.Graph           as APIGraph
import           Empire.API.Data.GraphLocation   (GraphLocation (..))
import           Empire.API.Data.Node            (ExpressionNode (..), InputSidebar (..), OutputSidebar (..), NodeId)
import qualified Empire.API.Data.Node            as Node
import           Empire.API.Data.NodeLoc         (NodeLoc (..))
import qualified Empire.API.Data.NodeLoc         as NodeLoc
import           Empire.API.Data.NodeMeta        (NodeMeta)
import qualified Empire.API.Data.NodeMeta        as NodeMeta
import           Empire.API.Data.Port            (InPortId, OutPortId, InPort, OutPort, InPortIndex (..), OutPortIndex (..), AnyPortId (..))
import qualified Empire.API.Data.Port            as Port
import           Empire.API.Data.PortRef         (AnyPortRef (..), InPortRef (..), OutPortRef (..))
import qualified Empire.API.Data.PortRef         as PortRef
import           Empire.ASTOp                    (ASTOp, runASTOp, runAliasAnalysis)

import qualified Empire.ASTOps.Builder           as ASTBuilder
import qualified Empire.ASTOps.Deconstruct       as ASTDeconstruct
import qualified Empire.ASTOps.Modify            as ASTModify
import qualified Empire.ASTOps.Read              as ASTRead
import qualified Empire.ASTOps.Parse             as ASTParse
import qualified Empire.ASTOps.Print             as ASTPrint
import qualified Empire.ASTOps.Remove            as ASTRemove
import qualified Empire.Commands.AST             as AST
import           Empire.Commands.Breadcrumb      (withBreadcrumb)
import qualified Empire.Commands.GraphBuilder    as GraphBuilder
import qualified Empire.Commands.GraphUtils      as GraphUtils
import qualified Empire.Commands.Library         as Library
import qualified Empire.Commands.Publisher       as Publisher
import           Empire.Empire

import qualified Luna.IR                          as IR
import qualified OCI.IR.Combinators               as IR (replaceSource, deleteSubtree, narrowTerm)
import           Luna.Syntax.Text.Parser.Marker   (MarkedExprMap(..))
import qualified Luna.Syntax.Text.Parser.Marker   as Luna
import qualified Luna.Syntax.Text.Parser.Parser   as Parser (ReparsingStatus(..), ReparsingChange(..))


generateNodeName :: ASTOp m => m String
generateNodeName = do
    lastNameId <- use Graph.lastNameId
    let newNameId = lastNameId + 1
    Graph.lastNameId .= newNameId
    return $ "node" <> show newNameId

addNodeCondTC :: Bool -> GraphLocation -> NodeId -> Text -> NodeMeta -> Empire ExpressionNode
addNodeCondTC True  loc uuid expr meta = addNode loc uuid expr meta
addNodeCondTC False loc uuid expr meta = withGraph loc $ addNodeNoTC loc uuid expr meta

addNode :: GraphLocation -> NodeId -> Text -> NodeMeta -> Empire ExpressionNode
addNode loc uuid expr meta = withTC loc False $ addNodeNoTC loc uuid expr meta

addNodeNoTC :: GraphLocation -> NodeId -> Text -> NodeMeta -> Command Graph ExpressionNode
addNodeNoTC loc uuid input meta = do
    parse <- fst <$> ASTParse.runParser input
    expr <- runASTOp $ do
        newNodeName  <- generateNodeName
        parsedNode   <- AST.addNode uuid newNodeName parse
        putIntoHierarchy uuid $ BH.MatchNode parsedNode
        return parsedNode
    runAliasAnalysis
    node <- runASTOp $ do
        putChildrenIntoHierarchy uuid expr
        AST.writeMeta expr meta
        updateNodeSequence
        GraphBuilder.buildNode uuid
    Publisher.notifyNodeUpdate loc node
    return node

prepareLambdaChild :: ASTOp m => BH.NodeIDTarget -> NodeRef -> m (Maybe BH.LamItem)
prepareLambdaChild tgt ref = do
    parsedIsLambda <- ASTRead.isLambda ref
    if parsedIsLambda then do
        portMapping            <- liftIO $ (,) <$> UUID.nextRandom <*> UUID.nextRandom
        lambdaOutput           <- ASTRead.getLambdaOutputRef   ref
        lambdaBody             <- ASTRead.getFirstNonLambdaRef ref
        outputIsOneOfTheInputs <- AST.isTrivialLambda ref
        ASTBuilder.attachNodeMarkersForArgs (fst $ portMapping) [] ref
        lambdaUUID <- liftIO $ UUID.nextRandom
        anonOutput <- if not outputIsOneOfTheInputs
            then do
                IR.putLayer @Marker lambdaOutput $ Just $ OutPortRef (NodeLoc def lambdaUUID) []
                return $ Just $ BH.ExprChild $ BH.ExprItem Map.empty (BH.AnonymousNode lambdaOutput)
            else return Nothing
        let lamItem = BH.LamItem portMapping tgt (Map.empty & at lambdaUUID .~ anonOutput) lambdaBody
        return $ Just lamItem
    else return Nothing

prepareExprChild :: ASTOp m => BH.NodeIDTarget -> NodeRef -> m (Maybe BH.ExprItem)
prepareExprChild tgt ref = do
    parsedIsLambda <- ASTRead.isLambda ref
    if not parsedIsLambda then do
        let bareItem = BH.ExprItem Map.empty tgt
        args  <- ASTDeconstruct.extractAppArguments ref
        items <- mapM (uncurry prepareLambdaChild . (BH.AnonymousNode &&& id)) args
        let addItem par (port, child) = case child of
              Just ch -> par & BH.portChildren . at port ?~ ch
              _       -> par
        return $ Just $ foldl addItem bareItem $ zip [0..] items
    else return Nothing

updateNodeSequenceWithOutput :: ASTOp m => Maybe NodeRef -> m ()
updateNodeSequenceWithOutput outputRef = do
    newSeq <- makeCurrentSeq outputRef
    updateGraphSeq newSeq

updateNodeSequence :: ASTOp m => m ()
updateNodeSequence = do
    currentTgt <- ASTRead.getCurrentASTTarget
    outputRef  <- mapM ASTRead.getLambdaOutputRef currentTgt
    updateNodeSequenceWithOutput outputRef

makeCurrentSeq :: ASTOp m => Maybe NodeRef -> m (Maybe NodeRef)
makeCurrentSeq out = do
  allNodes    <- uses Graph.breadcrumbHierarchy BH.topLevelIDs
  sortedRefs  <- AST.sortByPosition allNodes
  let withOut = fmap head $ group $ sortedRefs ++ toList out
  AST.makeSeq withOut

transplantExprMap :: ASTOp m => Maybe NodeRef -> Maybe NodeRef -> m ()
transplantExprMap (Just oldSeq) (Just newSeq) = do
    exprMap <- IR.getLayer @CodeMarkers oldSeq
    IR.putLayer @CodeMarkers newSeq exprMap
transplantExprMap _             _             = return ()

updateGraphSeq :: ASTOp m => Maybe NodeRef -> m ()
updateGraphSeq newOut = do
    oldSeq     <- preuse $ Graph.breadcrumbHierarchy . BH.body
    currentTgt <- ASTRead.getCurrentASTTarget
    outLink    <- mapM ASTRead.getFirstNonLambdaLink currentTgt
    case (,) <$> outLink <*> newOut of
        Just (l, o) -> IR.replaceSource o l
        Nothing     -> return ()
    transplantExprMap oldSeq newOut
    when (newOut /= oldSeq) $ mapM_ ASTRemove.removeSubtree oldSeq
    Graph.breadcrumbHierarchy . BH._ToplevelParent . BH.topBody .= newOut
    forM_ newOut $ (Graph.breadcrumbHierarchy . BH.body .=)

addPort :: GraphLocation -> NodeId -> Int -> Empire InputSidebar
addPort loc nid position = withTC loc False $ addPortNoTC loc nid position

addPortNoTC :: GraphLocation -> NodeId -> Int -> Command Graph InputSidebar
addPortNoTC loc nid position = runASTOp $ do
    edges <- GraphBuilder.getEdgePortMapping
    when ((fst <$> edges) /= Just nid) $ throwM NotInputEdgeException
    Just ref <- ASTRead.getCurrentASTTarget
    ASTBuilder.detachNodeMarkersForArgs ref
    ASTModify.addLambdaArg position ref
    newLam  <- ASTRead.getCurrentASTTarget
    mapM_ (ASTBuilder.attachNodeMarkersForArgs nid []) newLam
    GraphBuilder.buildInputSidebar nid

addPortWithConnection :: GraphLocation -> NodeId -> Int -> [Connection] -> Empire InputSidebar
addPortWithConnection loc nid position conns = withTC loc False $ do
    newPorts <- addPortNoTC loc nid position
    forM_ conns $ \(Connection src dst) -> connectNoTC loc src (InPortRef' dst)
    return newPorts


generateNodeId :: IO NodeId
generateNodeId = UUID.nextRandom

addSubgraph :: GraphLocation -> [ExpressionNode] -> [Connection] -> Empire [ExpressionNode]
addSubgraph loc nodes conns = withTC loc False $ do
    newNodes <- forM nodes $ \n -> addNodeNoTC loc (n ^. Node.nodeId) (n ^. Node.expression) (n ^. Node.nodeMeta)
    forM_ conns $ \(Connection src dst) -> connectNoTC loc src (InPortRef' dst)
    return newNodes

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

removePort :: GraphLocation -> OutPortRef -> Empire InputSidebar
removePort loc portRef = withGraph loc $ runASTOp $ do
    let nodeId = portRef ^. PortRef.srcNodeId
    Just ref    <- ASTRead.getCurrentASTTarget
    ASTBuilder.detachNodeMarkersForArgs ref
    edges <- GraphBuilder.getEdgePortMapping
    newRef <- case edges of
        Just (input, _output) -> do
            if nodeId == input then ASTModify.removeLambdaArg (portRef ^. PortRef.srcPortId) ref
                               else throwM NotInputEdgeException
        _ -> return ref
    when (ref /= newRef) $ ASTModify.rewireCurrentNode newRef
    ASTBuilder.attachNodeMarkersForArgs nodeId [] newRef
    GraphBuilder.buildInputSidebar nodeId

movePort :: GraphLocation -> OutPortRef -> Int -> Empire InputSidebar
movePort loc portRef newPosition = withGraph loc $ runASTOp $ do
    let nodeId = portRef ^. PortRef.srcNodeId
    Just ref    <- ASTRead.getCurrentASTTarget
    edges       <- GraphBuilder.getEdgePortMapping
    newRef      <- case edges of
        Just (input, _) -> do
            if nodeId == input then ASTModify.moveLambdaArg (portRef ^. PortRef.srcPortId) newPosition ref
                               else throwM NotInputEdgeException
        _ -> throwM NotInputEdgeException
    when (ref /= newRef) $ ASTModify.rewireCurrentNode newRef
    ASTBuilder.attachNodeMarkersForArgs nodeId [] ref
    GraphBuilder.buildInputSidebar nodeId

renamePort :: GraphLocation -> OutPortRef -> String -> Empire InputSidebar
renamePort loc portRef newName = withGraph loc $ runASTOp $ do
    let nodeId = portRef ^. PortRef.srcNodeId
    Just ref    <- ASTRead.getCurrentASTTarget
    edges       <- GraphBuilder.getEdgePortMapping
    _newRef     <- case edges of
        Just (input, _) -> do
            if nodeId == input then ASTModify.renameLambdaArg (portRef ^. PortRef.srcPortId) newName ref
                               else throwM NotInputEdgeException
        _ -> throwM NotInputEdgeException
    GraphBuilder.buildInputSidebar nodeId

setNodeExpression :: GraphLocation -> NodeId -> Text -> Empire ExpressionNode
setNodeExpression loc nodeId expression = withTC loc False $ do
    oldExpr    <- runASTOp $ ASTRead.getASTTarget nodeId
    parsedRef  <- view _1 <$> ASTParse.runReparser expression oldExpr
    runASTOp $ ASTModify.rewireNode nodeId parsedRef
    runAliasAnalysis
    node <- runASTOp $ do
        expr      <- ASTRead.getASTPointer nodeId
        lamItem   <- prepareLambdaChild (BH.MatchNode expr) parsedRef
        exprItem  <- prepareExprChild   (BH.MatchNode expr) parsedRef
        forM_ lamItem  $ (Graph.breadcrumbHierarchy . BH.children . ix nodeId .=) . BH.LambdaChild
        forM_ exprItem $ (Graph.breadcrumbHierarchy . BH.children . ix nodeId .=) . BH.ExprChild
        updateNodeSequence
        GraphBuilder.buildNode nodeId
    Publisher.notifyNodeUpdate loc node
    return node

setNodeMeta :: GraphLocation -> NodeId -> NodeMeta -> Empire ()
setNodeMeta loc nodeId newMeta = withGraph loc $ do
    doTCMay <- runASTOp $ do
        ref <- GraphUtils.getASTPointer nodeId
        oldMetaMay <- AST.readMeta ref
        let triggerTC :: NodeMeta -> NodeMeta -> Bool
            triggerTC oldMeta' newMeta' = oldMeta' ^. NodeMeta.displayResult /= newMeta' ^. NodeMeta.displayResult
        doTCMay <- forM oldMetaMay $ \oldMeta ->
            return $ triggerTC oldMeta newMeta
        AST.writeMeta ref newMeta
        updateNodeSequence
        return doTCMay
    forM_ doTCMay $ \doTC ->
        when doTC $ runTC loc False

connectCondTC :: Bool -> GraphLocation -> OutPortRef -> AnyPortRef -> Empire Connection
connectCondTC True  loc outPort anyPort = connect loc outPort anyPort
connectCondTC False loc outPort anyPort = withGraph loc $ connectNoTC loc outPort anyPort

connect :: GraphLocation -> OutPortRef -> AnyPortRef -> Empire Connection
connect loc outPort anyPort = withTC loc False $ connectNoTC loc outPort anyPort

connectPersistent :: ASTOp m => OutPortRef -> AnyPortRef -> m Connection
connectPersistent src@(OutPortRef (NodeLoc _ srcNodeId) srcPort) (InPortRef' dst@(InPortRef (NodeLoc _ dstNodeId) dstPort)) = do
    case dstPort of
        []        -> makeWhole srcNodeId dstNodeId srcPort
        [Self]    -> makeAcc   srcNodeId dstNodeId srcPort
        [Arg num] -> makeApp   srcNodeId dstNodeId num srcPort
    return $ Connection src dst
connectPersistent src@(OutPortRef (NodeLoc _ srcNodeId) srcPort) (OutPortRef' dst@(OutPortRef d@(NodeLoc _ dstNodeId) dstPort)) = do
    case dstPort of
        []    -> do
            ASTBuilder.flipNode dstNodeId
            updateNodeSequence
            connectPersistent src (InPortRef' (InPortRef d []))
        _ : _ -> throwM InvalidConnectionException

connectNoTC :: GraphLocation -> OutPortRef -> AnyPortRef -> Command Graph Connection
connectNoTC loc outPort anyPort = do
    (connection, nodeToUpdate) <- runASTOp $ do
        connection <- connectPersistent outPort anyPort

        -- if input port is not an edge, send update to gui
        edges <- GraphBuilder.getEdgePortMapping
        let nid = PortRef.nodeId' anyPort
        let nodeToUpdate = case edges of
                Just (input, output) -> do
                    if (nid /= input && nid /= output) then Just <$> GraphBuilder.buildNode nid
                                                       else return Nothing
                _ -> Just <$> GraphBuilder.buildNode nid
        (connection,) <$> nodeToUpdate
    forM_ nodeToUpdate $ \n -> do
        Publisher.notifyNodeUpdate loc n
    return connection

getPortDefault :: GraphLocation -> InPortRef -> Empire PortDefault
getPortDefault loc (InPortRef  _ (Self : _))                   = throwError "Cannot set default value on self port"
getPortDefault loc (InPortRef  (NodeLoc _ nodeId) (Arg x : _)) = withGraph loc $ runASTOp $ flip GraphBuilder.getInPortDefault x =<< GraphUtils.getASTTarget nodeId

setPortDefault :: GraphLocation -> InPortRef -> PortDefault -> Empire ()
setPortDefault loc (InPortRef (NodeLoc _ nodeId) port) val = withTC loc False $ runASTOp $ do
    parsed <- ASTParse.parsePortDefault val
    ref <- GraphUtils.getASTTarget nodeId
    newRef <- case port of
        [Self]    -> ASTBuilder.makeAccessor parsed ref
        [Arg num] -> ASTBuilder.applyFunction ref parsed num
    GraphUtils.rewireNode nodeId newRef

disconnect :: GraphLocation -> InPortRef -> Empire ()
disconnect loc port@(InPortRef (NodeLoc _ nid) _) = withTC loc False $ do
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

getBuffer :: FilePath -> Maybe (Int, Int) -> Empire Text
getBuffer file span = Library.getBuffer file span

getGraph :: GraphLocation -> Empire APIGraph.Graph
getGraph loc = withTC loc True $ runASTOp GraphBuilder.buildGraph

getNodes :: GraphLocation -> Empire [ExpressionNode]
getNodes loc = withTC loc True $ runASTOp $ view APIGraph.nodes <$> GraphBuilder.buildGraph

getConnections :: GraphLocation -> Empire [(OutPortRef, InPortRef)]
getConnections loc = withTC loc True $ runASTOp $ view APIGraph.connections <$> GraphBuilder.buildGraph

decodeLocation :: GraphLocation -> Empire (Breadcrumb (Named BreadcrumbItem))
decodeLocation loc@(GraphLocation file crumbs) =
    withGraph (GraphLocation file $ Breadcrumb []) $ GraphBuilder.decodeBreadcrumbs crumbs

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

openFile :: FilePath -> Empire ()
openFile path = do
    code <- do
        rawCode <- liftIO $ Text.readFile path
        return $ Text.stripEnd rawCode
    Library.createLibrary Nothing path code
    let loc = GraphLocation path $ Breadcrumb []
    withGraph loc $ loadCode code

typecheck :: GraphLocation -> Empire ()
typecheck loc = withGraph loc $ runTC loc False

substituteCode :: FilePath -> Int -> Int -> Text -> Maybe Int -> Empire (Maybe Parser.ReparsingStatus)
substituteCode path start end code cursor = do
    newCode <- Library.withLibrary path $ do
        currentCode <- use Library.code
        let len            = end - start
            (prefix, rest) = Text.splitAt start currentCode
            suffix         = Text.drop len rest
            newCode        = Text.concat [prefix, code, suffix]
        Library.code .= newCode
        return newCode
    let loc = GraphLocation path (Breadcrumb [])
    withTC loc True $ reloadCode loc newCode

reloadCode :: GraphLocation -> Text -> Command Graph (Maybe Parser.ReparsingStatus)
reloadCode loc code = handle (\(_e :: ASTParse.ParserException SomeException) -> return Nothing) $ do
    expr <- preuse $ Graph.breadcrumbHierarchy . BH.body
    case expr of
        Just e -> do
            (ref, exprMap, rs) <- ASTParse.runUnitReparser code e
            Graph.breadcrumbHierarchy . BH.body .= ref
            runASTOp $ forM_ (coerce rs :: [Parser.ReparsingChange]) $ \x -> case x of
                Parser.AddedExpr expr -> do
                    void $ insertNode expr
                Parser.ChangedExpr oldExpr expr -> do
                    oldNodeId <- ASTRead.safeGetVarNodeId oldExpr

                    forM_ oldNodeId $ \nodeId -> do
                        copyMeta oldExpr expr
                        markNode expr nodeId
                        putIntoHierarchy nodeId $ BH.MatchNode expr
                        ASTModify.rewireNode nodeId =<< ASTRead.getTargetNode expr
                Parser.UnchangedExpr oldExpr expr -> do
                    oldNodeId <- ASTRead.safeGetVarNodeId oldExpr

                    forM_ oldNodeId $ \nodeId -> do
                        copyMeta oldExpr expr
                        markNode expr nodeId
                        putIntoHierarchy nodeId $ BH.MatchNode expr
                Parser.RemovedExpr oldExpr -> do
                    oldNodeId <- ASTRead.safeGetVarNodeId oldExpr
                    forM_ oldNodeId $ removeNodeNoTC
            runAliasAnalysis
            return $ Just rs
        Nothing -> return Nothing

putIntoHierarchy :: ASTOp m => NodeId -> BH.NodeIDTarget -> m ()
putIntoHierarchy nodeId target = do
    let nodeItem = BH.ExprItem Map.empty target
    Graph.breadcrumbHierarchy . BH.children . at nodeId ?= BH.ExprChild nodeItem

putChildrenIntoHierarchy :: ASTOp m => NodeId -> NodeRef -> m ()
putChildrenIntoHierarchy uuid expr = do
    target   <- ASTRead.getASTTarget uuid
    lamItem  <- prepareLambdaChild (BH.MatchNode expr) target
    exprItem <- prepareExprChild   (BH.MatchNode expr) target
    forM_ lamItem  $ (Graph.breadcrumbHierarchy . BH.children . ix uuid .=) . BH.LambdaChild
    forM_ exprItem $ (Graph.breadcrumbHierarchy . BH.children . ix uuid .=) . BH.ExprChild

copyMeta :: ASTOp m => NodeRef -> NodeRef -> m ()
copyMeta donor recipient = do
    meta <- AST.readMeta donor
    forM_ meta $ AST.writeMeta recipient

markNode :: ASTOp m => NodeRef -> NodeId -> m ()
markNode expr nodeId = do
    var <- ASTRead.getVarNode expr
    ASTBuilder.attachNodeMarkers nodeId [] var

insertNode :: ASTOp m => NodeRef -> m NodeId
insertNode expr = do
    uuid <- liftIO $ UUID.nextRandom
    assignment <- ASTRead.isMatch expr
    if assignment then do
        markNode expr uuid
        putIntoHierarchy uuid $ BH.MatchNode expr
        putChildrenIntoHierarchy uuid expr
    else do
        putIntoHierarchy uuid $ BH.AnonymousNode expr
    return uuid

loadCode :: Text -> Command Graph ()
loadCode code = do
    (ref, exprMap) <- ASTParse.runUnitParser code
    runASTOp $ forM_ (coerce exprMap :: Map.Map Luna.Marker NodeRef) $ insertNode
    Graph.breadcrumbHierarchy . BH._ToplevelParent . BH.topBody .= Just ref
    Graph.breadcrumbHierarchy . BH.body .= ref
    runAliasAnalysis

printMarkedExpression :: ASTOp m => Map.Map Luna.Marker NodeRef -> NodeRef -> m Text
printMarkedExpression exprMap ref = do
    expr <- Text.pack <$> ASTPrint.printExpression ref
    let chunks = Text.splitOn " = " expr
    case chunks of
        [e] -> return e
        [var, val] -> do
            let markers = Map.keys $ Map.filter (== ref) exprMap
            case markers of
                [index] -> do
                    let marker = " ‹" ++ show index ++ "›= "
                    return $ Text.concat [var, Text.pack marker, val]
                _   -> return $ Text.concat [var, val]
        _ -> error "printMarkedExpression: bigger mistake"

updateCode :: GraphLocation -> Empire ()
updateCode loc@(GraphLocation file _) = do
    newCode <- withGraph loc $ runASTOp $ do
        Just nodeSeq <- GraphBuilder.getNodeSeq
        exprMap      <- IR.getLayer @CodeMarkers nodeSeq
        nodeSequence <- GraphBuilder.getNodeIdSequence
        refs         <- mapM ASTRead.getASTPointer nodeSequence
        expressions  <- mapM (printMarkedExpression (coerce exprMap)) refs
        let newCode = Text.unlines $ expressions
        return newCode
    Library.withLibrary file $ Library.code .= newCode
    Publisher.notifyCodeUpdate file 0 (Text.length newCode) newCode Nothing

-- internal

runTC :: GraphLocation -> Bool -> Command Graph ()
runTC loc flush = do
    g <- get
    Publisher.requestTC loc g flush

printNodeLine :: ASTOp m => NodeId -> m String
printNodeLine nodeId = GraphUtils.getASTPointer nodeId >>= ASTPrint.printExpression

withTC :: GraphLocation -> Bool -> Command Graph a -> Empire a
withTC loc@(GraphLocation file _) flush cmd = do
    res <- withGraph loc $ cmd
    withGraph (GraphLocation file $ Breadcrumb []) $ runTC loc flush
    return res

withGraph :: GraphLocation -> Command Graph a -> Empire a
withGraph (GraphLocation file breadcrumb) = withBreadcrumb file breadcrumb

getOutEdges :: ASTOp m => NodeId -> m [InPortRef]
getOutEdges nodeId = do
    edges <- GraphBuilder.buildConnections
    let filtered = filter (\(opr, _) -> opr ^. PortRef.srcNodeId == nodeId) edges
    return $ view _2 <$> filtered

disconnectPort :: ASTOp m => InPortRef -> m ()
disconnectPort (InPortRef (NodeLoc _ dstNodeId) dstPort) = case dstPort of
    []        -> setToNothing dstNodeId
    [Self]    -> unAcc dstNodeId
    [Arg num] -> unApp dstNodeId num

setToNothing :: ASTOp m => NodeId -> m ()
setToNothing dst = do
    edges <- GraphBuilder.getEdgePortMapping
    let disconnectOutputEdge = case edges of
            Nothing       -> False
            Just (_, out) -> out == dst
    nothing <- IR.generalize <$> IR.cons_ "None"
    if disconnectOutputEdge then updateNodeSequenceWithOutput (Just nothing) else GraphUtils.rewireNode dst nothing

unAcc :: ASTOp m => NodeId -> m ()
unAcc nodeId = do
    dstAst     <- GraphUtils.getASTTarget   nodeId
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

makeAcc :: ASTOp m => NodeId -> NodeId -> OutPortId -> m ()
makeAcc src dst outPort = do
    srcAst     <- ASTRead.getASTOutForPort src outPort
    dstAst     <- ASTRead.getASTTarget dst
    newNodeRef <- ASTBuilder.makeAccessor srcAst dstAst
    GraphUtils.rewireNode dst newNodeRef

makeApp :: ASTOp m => NodeId -> NodeId -> Int -> OutPortId -> m ()
makeApp src dst pos outPort = do
    srcAst     <- ASTRead.getASTOutForPort src outPort
    dstAst     <- GraphUtils.getASTTarget dst
    newNodeRef <- ASTBuilder.applyFunction dstAst srcAst pos
    GraphUtils.rewireNode dst newNodeRef

makeWhole :: ASTOp m => NodeId -> NodeId -> OutPortId -> m ()
makeWhole src dst outPort = do
    edges <- GraphBuilder.getEdgePortMapping
    let connectToOutputEdge = case edges of
            Nothing       -> False
            Just (_, out) -> out == dst
    srcAst <- ASTRead.getASTOutForPort src outPort
    if connectToOutputEdge then updateNodeSequenceWithOutput (Just srcAst) else GraphUtils.rewireNode dst srcAst

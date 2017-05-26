{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TupleSections         #-}

module Empire.Commands.Graph
    ( addNode
    , addNodeCondTC
    , addPort
    , addPortWithConnections
    , addSubgraph
    , autolayoutNodes
    , removeNodes
    , movePort
    , removePort
    , renamePort
    , setNodeExpression
    , setNodeMeta
    , setNodePosition
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
    , getGraphNoTC
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
    , markerCodeSpan
    , getNodeIdForMarker
    , withTC
    , withGraph
    , runTC
    ) where

import           Control.Exception             (evaluate)
import           Control.Monad                 (forM, forM_)
import           Control.Monad.Catch           (MonadCatch(..), catchAll, handle, onException)
import           Control.Monad.State           hiding (when)
import           Control.Arrow                 ((&&&))
import           Control.Monad.Error           (throwError)
import           Data.Coerce                   (coerce)
import           Data.List                     (delete, elemIndex, sort, sortOn, group)
import qualified Data.Map                      as Map
import           Data.Map                      (Map)
import qualified Data.Set                      as Set
import           Data.Maybe                    (catMaybes, fromMaybe, isJust, listToMaybe, maybeToList)
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
                                                  InvalidConnectionException (..), SomeASTException,
                                                  astExceptionFromException, astExceptionToException)
import qualified Empire.Data.BreadcrumbHierarchy as BH
import           Empire.Data.Graph               (Graph)
import qualified Empire.Data.Graph               as Graph
import qualified Empire.Data.Library             as Library
import           Empire.Data.Layers              (CodeMarkers, Marker)

import           LunaStudio.Data.Breadcrumb      (Breadcrumb (..), Named, BreadcrumbItem)
import qualified LunaStudio.Data.Breadcrumb      as Breadcrumb
import qualified LunaStudio.Data.Connection      as Connection
import           LunaStudio.Data.Connection      (Connection (..))
import           LunaStudio.Data.PortDefault     (PortDefault (Constant))
import qualified LunaStudio.Data.Graph           as APIGraph
import           LunaStudio.Data.GraphLocation   (GraphLocation (..))
import           LunaStudio.Data.Node            (ExpressionNode (..), InputSidebar (..), OutputSidebar (..), NodeId)
import qualified LunaStudio.Data.Node            as Node
import           LunaStudio.Data.NodeLoc         (NodeLoc (..))
import qualified LunaStudio.Data.NodeLoc         as NodeLoc
import           LunaStudio.Data.NodeMeta        (NodeMeta)
import qualified LunaStudio.Data.NodeMeta        as NodeMeta
import           LunaStudio.Data.Port            (InPortId, OutPortId, InPort, OutPort, InPortIndex (..), OutPortIndex (..), AnyPortId (..), getPortNumber)
import qualified LunaStudio.Data.Port            as Port
import           LunaStudio.Data.PortRef         (AnyPortRef (..), InPortRef (..), OutPortRef (..))
import qualified LunaStudio.Data.PortRef         as PortRef
import           LunaStudio.Data.Position        (Position)
import qualified LunaStudio.Data.Position        as Position
import           Empire.ASTOp                    (ASTOp, putNewIR, runASTOp, runAliasAnalysis)

import qualified Empire.ASTOps.Builder           as ASTBuilder
import qualified Empire.ASTOps.Deconstruct       as ASTDeconstruct
import qualified Empire.ASTOps.Modify            as ASTModify
import qualified Empire.ASTOps.Read              as ASTRead
import qualified Empire.ASTOps.Parse             as ASTParse
import qualified Empire.ASTOps.Print             as ASTPrint
import qualified Empire.ASTOps.Remove            as ASTRemove
import qualified Empire.Commands.AST             as AST
import qualified Empire.Commands.Autolayout      as Autolayout
import           Empire.Commands.Breadcrumb      (withBreadcrumb)
import qualified Empire.Commands.GraphBuilder    as GraphBuilder
import qualified Empire.Commands.GraphUtils      as GraphUtils
import qualified Empire.Commands.Lexer           as Lexer
import qualified Empire.Commands.Library         as Library
import qualified Empire.Commands.Publisher       as Publisher
import           Empire.Empire

import           Data.Text.Position               (Delta)
import           Data.SpanTree                    (LeftSpacedSpan(..))
import qualified Luna.IR                          as IR
import qualified OCI.IR.Combinators               as IR (replaceSource, deleteSubtree, narrowTerm)
import           Luna.Syntax.Text.Parser.CodeSpan (CodeSpan)
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
addNodeCondTC False loc uuid expr meta = do
    (nearestNode, node) <- withGraph loc $ addNodeNoTC loc uuid expr Nothing meta
    addToCode loc nearestNode $ node ^. Node.nodeId
    resendCode loc
    return node

addNode :: GraphLocation -> NodeId -> Text -> NodeMeta -> Empire ExpressionNode
addNode loc uuid expr meta = addNodeWithName loc uuid expr Nothing meta

addNodeWithName :: GraphLocation -> NodeId -> Text -> Maybe Text -> NodeMeta -> Empire ExpressionNode
addNodeWithName loc uuid expr name meta = do
    (nearestNode, node) <- withTC loc False $ addNodeNoTC loc uuid expr Nothing meta
    addToCode loc nearestNode $ node ^. Node.nodeId
    resendCode loc
    return node

addNodeNoTC :: GraphLocation -> NodeId -> Text -> Maybe Text -> NodeMeta -> Command Graph (Maybe NodeRef, ExpressionNode)
addNodeNoTC loc uuid input name meta = do
    parse <- fst <$> ASTParse.runParser input
    (nearestNode, expr) <- runASTOp $ do
        newNodeName  <- case name of
            Just n -> return $ Text.unpack n
            _      -> generateNodeName
        parsedNode   <- AST.addNode uuid newNodeName parse
        index        <- getNextExprMarker
        IR.putLayer @CodeSpan parsedNode (Just $ LeftSpacedSpan 5 (fromIntegral $ Text.length input + length newNodeName + length (show index) + 2 + 2 + 1))
        putIntoHierarchy uuid $ BH.MatchNode parsedNode
        nearestNode  <- putInSequence parsedNode meta
        addExprMapping index parsedNode
        return (nearestNode, parsedNode)
    runAliasAnalysis
    node <- runASTOp $ do
        putChildrenIntoHierarchy uuid expr
        AST.writeMeta expr meta
        node <- GraphBuilder.buildNode uuid
        return node
    return (nearestNode, node)

distanceTo :: (Double, Double) -> (Double, Double) -> Double
distanceTo (xRef, yRef) (xPoint, yPoint) = sqrt $ (xRef - xPoint) ** 2 + (yRef - yPoint) ** 2

findPreviousNodeInSequence :: ASTOp m => NodeMeta -> [(NodeRef, NodeMeta)] -> m (Maybe NodeRef)
findPreviousNodeInSequence meta nodes = do
    let position           = Position.toTuple $ view NodeMeta.position meta
        nodesWithPositions = map (\(n, m) -> (n, Position.toTuple $ m ^. NodeMeta.position)) nodes
        nodesToTheLeft     = filter (\(n, (x, y)) -> x < fst position || (x == fst position && y < snd position)) nodesWithPositions
        nearestNode        = listToMaybe $ sortOn (\(n, p) -> distanceTo position p) nodesToTheLeft
    return $ fmap fst nearestNode

findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM p [] = return Nothing
findM p (x:xs) = do
     b <- p x
     if b then return (Just x) else findM p xs

putInSequence :: ASTOp m => NodeRef -> NodeMeta -> m (Maybe NodeRef)
putInSequence ref meta = do
    oldSeq      <- preuse $ Graph.breadcrumbHierarchy . BH.body
    nearestNode <- case oldSeq of
        Just s -> do
            nodes              <- AST.readSeq s
            nodesAndMetas      <- mapM (\n -> (n,) <$> AST.readMeta n) nodes
            let nodesWithMetas =  mapMaybe (\(n,m) -> (n,) <$> m) nodesAndMetas
            nearestNode        <- findPreviousNodeInSequence meta nodesWithMetas
            seqs               <- AST.getSeqs s
            case nearestNode of
                Just n -> do
                    previous <- mapM AST.previousNodeForSeq seqs
                    seqToModify <- findM (\seq -> (== Just n) <$> AST.previousNodeForSeq seq) seqs
                    case seqToModify of
                        Just seq -> IR.matchExpr seq $ \case
                            IR.Seq l r -> do
                                l'     <- IR.source l
                                newSeq <- IR.generalize <$> IR.seq l' ref
                                IR.replaceSource newSeq l
                                void $ updateCodeSpan s
                            _       -> undefined
                        _        -> updateGraphSeq =<< AST.makeSeq (nodes ++ [ref])
                _ -> updateGraphSeq =<< AST.makeSeq (ref:nodes)
            Just newSeq <- preuse $ Graph.breadcrumbHierarchy . BH.body
            newNodes    <- AST.readSeq newSeq
            return nearestNode
        _ -> do
            updateGraphSeq =<< AST.makeSeq [ref]
            return Nothing
    return nearestNode

addToCode :: GraphLocation -> Maybe NodeRef -> NodeId -> Empire ()
addToCode loc@(GraphLocation file _) previous inserted = do
    (expr, position) <- withGraph loc $ runASTOp $ do
        ref   <- ASTRead.getASTPointer inserted
        expr  <- printMarkedExpression ref
        range <- readRange ref
        LeftSpacedSpan off _ <- readCodeSpan ref
        let offset = Text.replicate (fromIntegral off - 1) " "
        return (Text.concat [offset, expr], fst range - fromIntegral off + 1)
    Library.withLibrary file $ Library.applyDiff position position (Text.concat [expr, "\n"])
    return ()

addExprMapping :: ASTOp m => Word64 -> NodeRef -> m ()
addExprMapping index ref = do
    exprMap    <- getExprMap
    let newMap = exprMap & at index ?~ ref
    setExprMap newMap

getNextExprMarker :: ASTOp m => m Word64
getNextExprMarker = do
    exprMap <- getExprMap
    let keys         = Map.keys exprMap
        highestIndex = Safe.maximumMay keys
    return $ maybe 0 succ highestIndex

makeTopBreadcrumbHierarchy :: ASTOp m => NodeRef -> m BH.TopItem
makeTopBreadcrumbHierarchy ref = do
    let bareItem = BH.TopItem def $ Just ref
    children <- childrenFromSeq ref
    return $ bareItem & BH.children .~ children

childrenFromSeq :: ASTOp m => NodeRef -> m (Map NodeId BH.BChild)
childrenFromSeq ref = do
    IR.matchExpr ref $ \case
        IR.Seq   l r -> Map.union <$> (childrenFromSeq =<< IR.source l) <*> (childrenFromSeq =<< IR.source r)
        IR.Unify l r -> do
            uid   <- liftIO UUID.nextRandom
            ASTBuilder.attachNodeMarkers uid []      =<< IR.source l
            child <- prepareChild (BH.MatchNode ref) =<< IR.source r
            return $ Map.singleton uid child
        _ -> do
            uid   <- liftIO UUID.nextRandom
            IR.putLayer @Marker ref $ Just $ OutPortRef (NodeLoc def uid) []
            child <- prepareChild (BH.AnonymousNode ref) ref
            return $ Map.singleton uid child

lambdaChildren :: ASTOp m => NodeRef -> m (Map NodeId BH.BChild)
lambdaChildren ref = IR.matchExpr ref $ \case
    IR.Seq l r -> Map.union <$> (childrenFromSeq =<< IR.source l) <*> (lambdaChildren =<< IR.source r)
    _          -> do
        marker <- IR.getLayer @Marker ref
        case marker of
            Just a  -> return Map.empty
            Nothing -> childrenFromSeq ref

prepareChild :: ASTOp m => BH.NodeIDTarget -> NodeRef -> m BH.BChild
prepareChild tgt ref = do
    isLambda <- ASTRead.isLambda ref
    (if isLambda then fmap BH.LambdaChild .: prepareLambdaChild else prepareExprChild) tgt ref

prepareChildWhenLambda :: ASTOp m => BH.NodeIDTarget -> NodeRef -> m (Maybe BH.LamItem)
prepareChildWhenLambda tgt ref = do
    isLambda <- ASTRead.isLambda ref
    if isLambda then Just <$> prepareLambdaChild tgt ref else return Nothing

prepareLambdaChild :: ASTOp m => BH.NodeIDTarget -> NodeRef -> m BH.LamItem
prepareLambdaChild tgt ref = do
    portMapping  <- liftIO $ (,) <$> UUID.nextRandom <*> UUID.nextRandom
    lambdaOutput <- ASTRead.getLambdaOutputRef   ref
    lambdaBody   <- ASTRead.getFirstNonLambdaRef ref
    ASTBuilder.attachNodeMarkersForArgs (fst portMapping) [] ref
    children     <- lambdaChildren lambdaBody
    return $ BH.LamItem portMapping tgt children lambdaBody

prepareExprChild :: ASTOp m => BH.NodeIDTarget -> NodeRef -> m BH.BChild
prepareExprChild tgt ref = do
    let bareItem = BH.ExprItem Map.empty tgt
    args  <- ASTDeconstruct.extractAppArguments ref
    items <- mapM (uncurry prepareChildWhenLambda . (BH.AnonymousNode &&& id)) args
    let addItem par (port, child) = case child of
          Just ch -> par & BH.portChildren . at port ?~ ch
          _       -> par
    return $ BH.ExprChild $ foldl addItem bareItem $ zip [0..] items

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
    forM_ oldSeq $ flip IR.deepDeleteWithWhitelist $ Set.fromList $ maybeToList newOut
    Graph.breadcrumbHierarchy . BH._ToplevelParent . BH.topBody .= newOut
    forM_ newOut $ (Graph.breadcrumbHierarchy . BH.body .=)
    forM_ newOut $ updateCodeSpan

updateCodeSpan :: ASTOp m => NodeRef -> m (LeftSpacedSpan Delta)
updateCodeSpan ref = IR.matchExpr ref $ \case
    IR.Seq l r -> do
        l' <- updateCodeSpan =<< IR.source l
        r' <- updateCodeSpan =<< IR.source r
        IR.putLayer @CodeSpan ref (Just $ l' <> r')
        return (l' <> r')
    _          -> fromMaybe mempty <$> IR.getLayer @CodeSpan ref

addPort :: GraphLocation -> OutPortRef -> Empire InputSidebar
addPort loc portRef = withTC loc False $ addPortNoTC loc portRef

addPortNoTC :: GraphLocation -> OutPortRef -> Command Graph InputSidebar
addPortNoTC loc (OutPortRef nl pid) = runASTOp $ do
    let nid      = nl ^. NodeLoc.nodeId
        position = getPortNumber pid
    edges <- GraphBuilder.getEdgePortMapping
    when ((fst <$> edges) /= Just nid) $ throwM NotInputEdgeException
    Just ref <- ASTRead.getCurrentASTTarget
    ASTBuilder.detachNodeMarkersForArgs ref
    ASTModify.addLambdaArg position ref
    newLam  <- ASTRead.getCurrentASTTarget
    mapM_ (ASTBuilder.attachNodeMarkersForArgs nid []) newLam
    GraphBuilder.buildInputSidebar nid

addPortWithConnections :: GraphLocation -> OutPortRef -> [AnyPortRef] -> Empire InputSidebar
addPortWithConnections loc portRef connectTo = withTC loc False $ do
    newPorts <- addPortNoTC loc portRef
    forM_ connectTo $ connectNoTC loc portRef
    return newPorts


generateNodeId :: IO NodeId
generateNodeId = UUID.nextRandom

addSubgraph :: GraphLocation -> [ExpressionNode] -> [Connection] -> Empire [ExpressionNode]
addSubgraph loc nodes conns = withTC loc False $ do
    newNodes <- forM nodes $ \n -> addNodeNoTC loc (n ^. Node.nodeId) (n ^. Node.expression) (n ^. Node.name) (n ^. Node.nodeMeta)
    forM_ conns $ \(Connection src dst) -> connectNoTC loc src (InPortRef' dst)
    return $ map snd newNodes

removeNodes :: GraphLocation -> [NodeId] -> Empire ()
removeNodes loc@(GraphLocation file _) nodeIds = do
    forM_ nodeIds $ removeFromCode loc
    affectedNodes <- withTC loc False $ runASTOp $ mapM removeNodeNoTC nodeIds
    let distinctNodes = Set.toList $ Set.fromList $ concat affectedNodes
    forM_ distinctNodes $ updateNodeCode loc
    resendCode loc

removeFromCode :: GraphLocation -> NodeId -> Empire ()
removeFromCode loc@(GraphLocation file _) nodeId = do
    (start, end) <- withGraph loc $ runASTOp $ do
        ref   <- ASTRead.getASTPointer nodeId
        range <- readRange ref
        LeftSpacedSpan off _ <- readCodeSpan ref
        return (fst range - fromIntegral off, snd range)
    void $ Library.withLibrary file $ Library.applyDiff start end ""

removeNodeNoTC :: ASTOp m => NodeId -> m [NodeId]
removeNodeNoTC nodeId = do
    astRef        <- GraphUtils.getASTPointer nodeId
    obsoleteEdges <- getOutEdges nodeId
    mapM_ disconnectPort obsoleteEdges
    Graph.breadcrumbHierarchy . BH.children . at nodeId .= Nothing
    removeFromSequence astRef
    removeExprMarker astRef
    return $ map (view PortRef.dstNodeId) obsoleteEdges

removeExprMarker :: ASTOp m => NodeRef -> m ()
removeExprMarker ref = do
    exprMap <- getExprMap
    let newExprMap = Map.filter (/= ref) exprMap
    setExprMap newExprMap

removeFromSequence :: ASTOp m => NodeRef -> m ()
removeFromSequence ref = do
    Just oldSeq <- preuse $ Graph.breadcrumbHierarchy . BH.body
    nodes       <- AST.readSeq oldSeq
    let removed  = delete ref nodes
    newSeq      <- AST.makeSeq removed
    updateGraphSeq newSeq

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

renamePort :: GraphLocation -> OutPortRef -> Text -> Empire InputSidebar
renamePort loc portRef newName = withGraph loc $ runASTOp $ do
    let nodeId = portRef ^. PortRef.srcNodeId
    Just ref    <- ASTRead.getCurrentASTTarget
    edges       <- GraphBuilder.getEdgePortMapping
    _newRef     <- case edges of
        Just (input, _) -> do
            if nodeId == input then ASTModify.renameLambdaArg (portRef ^. PortRef.srcPortId) (Text.unpack newName) ref
                               else throwM NotInputEdgeException
        _ -> throwM NotInputEdgeException
    GraphBuilder.buildInputSidebar nodeId

makeTarget :: ASTOp m => NodeRef -> m BH.NodeIDTarget
makeTarget ref = IR.matchExpr ref $ \case
    IR.Unify{} -> return $ BH.MatchNode ref
    _          -> return $ BH.AnonymousNode ref

setNodeExpression :: GraphLocation -> NodeId -> Text -> Empire ExpressionNode
setNodeExpression loc@(GraphLocation file _) nodeId expression = do
    (node, oldRange, code) <- withTC loc False $ do
        oldExpr   <- runASTOp $ ASTRead.getASTTarget nodeId
        parsedRef <- view _1 <$> ASTParse.runReparser expression oldExpr
        oldRange  <- runASTOp $ do
            oldPointer <- ASTRead.getASTPointer nodeId
            oldRange   <- readRange oldPointer
            isMatch    <- ASTRead.isMatch oldPointer
            ASTModify.rewireNode nodeId parsedRef
            when (not isMatch) $ do
                putIntoHierarchy nodeId $ BH.AnonymousNode parsedRef
                updateExprMap parsedRef oldExpr
            return oldRange
        runAliasAnalysis
        (node, code)  <- runASTOp $ do
            expr      <- ASTRead.getASTPointer nodeId
            target    <- makeTarget expr
            item      <- prepareChild target parsedRef
            Graph.breadcrumbHierarchy . BH.children . ix nodeId .= item
            node <- GraphBuilder.buildNode nodeId
            code <- printMarkedExpression expr
            LeftSpacedSpan off _ <- readCodeSpan expr
            IR.putLayer @CodeSpan expr (Just $ LeftSpacedSpan off (fromIntegral $ Text.length code))
            oldSeq      <- preuse $ Graph.breadcrumbHierarchy . BH.body
            forM_ oldSeq updateCodeSpan
            return (node, code)
        return (node, oldRange, code)
    Library.withLibrary file $ Library.applyDiff (fst oldRange) (snd oldRange) code
    resendCode loc
    return node

updateExprMap :: ASTOp m => NodeRef -> NodeRef -> m ()
updateExprMap new old = do
    exprMap <- getExprMap
    let updated = Map.map (\a -> if a == old then new else a) exprMap
    setExprMap updated

resendCode :: GraphLocation -> Empire ()
resendCode (GraphLocation file _) = do
    code <- Library.withLibrary file $ use Library.code
    Publisher.notifyCodeUpdate file 0 (Text.length code) code Nothing

setNodeMeta :: GraphLocation -> NodeId -> NodeMeta -> Empire ()
setNodeMeta loc nodeId newMeta = withGraph loc $ runASTOp $ do
    ref <- GraphUtils.getASTPointer nodeId
    AST.writeMeta ref newMeta

setNodePosition :: GraphLocation -> NodeId -> Position -> Empire ()
setNodePosition loc nodeId newPos = do
    oldMeta <- fromMaybe def <$> getNodeMeta loc nodeId
    setNodeMeta loc nodeId $ oldMeta & NodeMeta.position .~ newPos

connectCondTC :: Bool -> GraphLocation -> OutPortRef -> AnyPortRef -> Empire Connection
connectCondTC True  loc outPort anyPort = connect loc outPort anyPort
connectCondTC False loc outPort anyPort = do
    connection <- withGraph loc $ connectNoTC loc outPort anyPort
    updateNodeCode loc $ anyPort ^. PortRef.nodeId
    resendCode loc
    return connection

connect :: GraphLocation -> OutPortRef -> AnyPortRef -> Empire Connection
connect loc outPort anyPort = do
    connection <- withTC loc False $ connectNoTC loc outPort anyPort
    updateNodeCode loc $ anyPort ^. PortRef.nodeId
    resendCode loc
    return connection

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
connectNoTC loc outPort anyPort = runASTOp $ connectPersistent outPort anyPort

data SelfPortDefaultException = SelfPortDefaultException InPortRef
    deriving (Show)

instance Exception SelfPortDefaultException where
    fromException = astExceptionFromException
    toException = astExceptionToException

getPortDefault :: GraphLocation -> InPortRef -> Empire (Maybe PortDefault)
getPortDefault loc port@(InPortRef  _ (Self : _))              = throwM $ SelfPortDefaultException port
getPortDefault loc (InPortRef  (NodeLoc _ nodeId) (Arg x : _)) = withGraph loc $ runASTOp $ flip GraphBuilder.getInPortDefault x =<< GraphUtils.getASTTarget nodeId

setPortDefault :: GraphLocation -> InPortRef -> Maybe PortDefault -> Empire ()
setPortDefault loc (InPortRef (NodeLoc _ nodeId) port) (Just val) = withTC loc False $ runASTOp $ do
    parsed <- ASTParse.parsePortDefault val
    ref    <- GraphUtils.getASTTarget nodeId
    newRef <- case port of
        [Self]    -> ASTBuilder.makeAccessor parsed ref
        [Arg num] -> ASTBuilder.applyFunction ref parsed num
    GraphUtils.rewireNode nodeId newRef
setPortDefault loc port Nothing = withTC loc False $ runASTOp $ disconnectPort port

disconnect :: GraphLocation -> InPortRef -> Empire ()
disconnect loc port@(InPortRef (NodeLoc _ nid) _) = do
    nodeId <- withTC loc False $ do
        nodeToUpdate <- runASTOp $ do
            disconnectPort port

            -- if input port is not an edge, send update to gui
            edges <- GraphBuilder.getEdgePortMapping
            case edges of
                Just (input, output) -> do
                    if (nid /= input && nid /= output) then Just <$> GraphBuilder.buildNode nid
                                                       else return Nothing
                _ -> Just <$> GraphBuilder.buildNode nid
        return $ view Node.nodeId <$> nodeToUpdate
    forM_ nodeId $ updateNodeCode loc
    resendCode loc

getNodeMeta :: GraphLocation -> NodeId -> Empire (Maybe NodeMeta)
getNodeMeta loc nodeId = withGraph loc $ runASTOp $ do
    ref <- GraphUtils.getASTPointer nodeId
    AST.readMeta ref

getCode :: GraphLocation -> Empire String
getCode loc@(GraphLocation file _) = Text.unpack <$> Library.withLibrary file (use Library.code)

getBuffer :: FilePath -> Maybe (Int, Int) -> Empire Text
getBuffer file span = Library.getBuffer file span

getGraph :: GraphLocation -> Empire APIGraph.Graph
getGraph loc = withTC loc True $ runASTOp $ do
    GraphBuilder.buildGraph

getGraphNoTC :: GraphLocation -> Empire APIGraph.Graph
getGraphNoTC loc = withGraph loc $ runASTOp $ GraphBuilder.buildGraph

getNodes :: GraphLocation -> Empire [ExpressionNode]
getNodes loc = withTC loc True $ runASTOp $ view APIGraph.nodes <$> GraphBuilder.buildGraph

getConnections :: GraphLocation -> Empire [(OutPortRef, InPortRef)]
getConnections loc = withTC loc True $ runASTOp $ view APIGraph.connections <$> GraphBuilder.buildGraph

decodeLocation :: GraphLocation -> Empire (Breadcrumb (Named BreadcrumbItem))
decodeLocation loc@(GraphLocation file crumbs) =
    withGraph (GraphLocation file $ Breadcrumb []) $ GraphBuilder.decodeBreadcrumbs crumbs

renameNode :: GraphLocation -> NodeId -> Text -> Empire ()
renameNode loc nid name = do
    withTC loc False $ do
        runASTOp $ renameNodeGraph nid name
        runAliasAnalysis
    updateNodeCode loc nid
    resendCode loc

renameNodeGraph :: ASTOp m => NodeId -> Text -> m ()
renameNodeGraph nid name = do
    vref <- GraphUtils.getASTVar nid
    ASTModify.renameVar vref (Text.unpack name)

dumpGraphViz :: GraphLocation -> Empire ()
dumpGraphViz loc = withGraph loc $ return ()

autolayoutNodes :: GraphLocation -> [NodeId] -> Empire ()
autolayoutNodes loc nids = do
    nodes <- getNodes loc
    conns <- getConnections loc
    mapM_ (uncurry $ setNodePosition loc) $ Autolayout.autolayoutNodes nids nodes conns

openFile :: FilePath -> Empire ()
openFile path = do
    code <- liftIO $ Text.readFile path
    Library.createLibrary Nothing path code
    let loc = GraphLocation path $ Breadcrumb []
    nodeIds <- withGraph loc $ do
        loadCode code
        uses Graph.breadcrumbHierarchy BH.topLevelIDs
    autolayoutNodes loc nodeIds

typecheck :: GraphLocation -> Empire ()
typecheck loc = withTC loc False $ return ()

substituteCode :: FilePath -> Int -> Int -> Text -> Maybe Int -> Empire ()
substituteCode path start end code cursor = do
    newCode <- Library.withLibrary path $ Library.applyDiff start end code
    let loc = GraphLocation path (Breadcrumb [])
    handle (\(_e :: SomeASTException) -> return ()) $ do
        withGraph loc $ reloadCode loc newCode

reloadCode :: GraphLocation -> Text -> Command Graph ()
reloadCode loc code = do
    oldMetas <- runASTOp $ do
        m <- getExprMap
        oldMetas <- forM (Map.assocs m) $ \(marker, expr) -> (marker,) <$> AST.readMeta expr
        return [ (marker, meta) | (marker, Just meta) <- oldMetas ]
    oldHierarchy <- use Graph.breadcrumbHierarchy
    Graph.breadcrumbHierarchy .= def
    loadCode code `onException` (Graph.breadcrumbHierarchy .= oldHierarchy)
    runASTOp $ do
        currentExprMap <- getExprMap
        forM_ oldMetas $ \(marker, oldMeta) -> do
            let expr = Map.lookup marker currentExprMap
            forM_ expr $ \e -> AST.writeMeta e oldMeta

putIntoHierarchy :: ASTOp m => NodeId -> BH.NodeIDTarget -> m ()
putIntoHierarchy nodeId target = do
    let nodeItem = BH.ExprItem Map.empty target
    Graph.breadcrumbHierarchy . BH.children . at nodeId ?= BH.ExprChild nodeItem

putChildrenIntoHierarchy :: ASTOp m => NodeId -> NodeRef -> m ()
putChildrenIntoHierarchy uuid expr = do
    target       <- ASTRead.getASTTarget uuid
    nodeIdTarget <- makeTarget expr
    item         <- prepareChild nodeIdTarget target
    Graph.breadcrumbHierarchy . BH.children . ix uuid .= item

copyMeta :: ASTOp m => NodeRef -> NodeRef -> m ()
copyMeta donor recipient = do
    meta <- AST.readMeta donor
    forM_ meta $ AST.writeMeta recipient

markNode :: ASTOp m => NodeId -> m ()
markNode nodeId = do
    var <- ASTRead.getASTMarkerPosition nodeId
    ASTBuilder.attachNodeMarkers nodeId [] var

insertNode :: ASTOp m => NodeRef -> m NodeId
insertNode expr = do
    uuid <- liftIO $ UUID.nextRandom
    assignment <- ASTRead.isMatch expr
    putIntoHierarchy uuid $ if assignment then BH.MatchNode expr else BH.AnonymousNode expr
    markNode uuid
    AST.writeMeta expr def
    return uuid

loadCode :: Text -> Command Graph ()
loadCode code | Text.null code = return ()
loadCode code = do
    (ir, IR.Rooted main ref, exprMap) <- liftIO $ ASTParse.runProperParser code
    Graph.unit .= ir
    putNewIR main
    Graph.breadcrumbHierarchy . BH._ToplevelParent . BH.topBody ?= ref
    runAliasAnalysis
    newBH <- runASTOp $ do
        IR.putLayer @CodeMarkers ref exprMap
        makeTopBreadcrumbHierarchy ref
    Graph.breadcrumbHierarchy .= BH.ToplevelParent newBH

nodeLineById :: ASTOp m => NodeId -> m (Maybe Int)
nodeLineById nodeId = do
    nodeIds <- GraphBuilder.getNodeIdSequence
    let line = elemIndex nodeId nodeIds
    return line

nodeLine :: ASTOp m => NodeRef -> m (Maybe Int)
nodeLine ref = do
    Just nodeSeq <- GraphBuilder.getNodeSeq
    nodes        <- AST.readSeq nodeSeq
    let line = elemIndex ref nodes
    return line

getExprMap :: ASTOp m => m (Map.Map Luna.Marker NodeRef)
getExprMap = do
    nodeSeq <- GraphBuilder.getNodeSeq
    case nodeSeq of
        Just s -> coerce <$> IR.getLayer @CodeMarkers s
        _      -> return Map.empty

setExprMap :: ASTOp m => Map.Map Luna.Marker NodeRef -> m ()
setExprMap exprMap = do
    nodeSeq <- GraphBuilder.getNodeSeq
    forM_ nodeSeq $ \s -> IR.putLayer @CodeMarkers s (coerce exprMap)

printMarkedExpression :: ASTOp m => NodeRef -> m Text
printMarkedExpression ref = do
    exprMap <- getExprMap
    expr <- Text.pack <$> ASTPrint.printExpression ref
    let chunks  = Text.splitOn " = " expr
        markers = Map.keys $ Map.filter (== ref) exprMap
        marker  = case markers of
            (index:_) -> Text.pack $ "«" ++ show index ++ "»"
            _         -> ""
    return $ case chunks of
        [e]        -> Text.concat [marker, e]
        [var, val] -> Text.concat [var, " ", marker, "= ", val]
        list       -> Text.concat list

isSidebar :: ASTOp m => NodeId -> m Bool
isSidebar nodeId = do
    sidebars <- GraphBuilder.getEdgePortMapping
    case sidebars of
        Nothing              -> return False
        Just (input, output) -> return $ input == nodeId || output == nodeId

updateNodeCode :: GraphLocation -> NodeId -> Empire ()
updateNodeCode loc@(GraphLocation file _) nodeId = do
    sidebar <- withGraph loc $ runASTOp $ isSidebar nodeId
    if sidebar then return () else do
        (range, expression) <- withGraph loc $ runASTOp $ do
            ref        <- ASTRead.getASTPointer nodeId
            expression <- printMarkedExpression ref
            range      <- readRange ref
            return (range, expression)
        void $ Library.withLibrary file $ Library.applyDiff (fst range) (snd range + 1) $ Text.concat [expression, "\n"]

readRange' :: ASTOp m => NodeRef -> m (LeftSpacedSpan Delta)
readRange' ref = IR.matchExpr ref $ \case
    IR.Seq{} -> return mempty
    _        -> do
        parents <- IR.getLayer @IR.Succs ref
        case toList parents of
            []       -> return mempty
            [parent] -> do
                inputs <- mapM IR.source =<< IR.inputs =<< IR.readTarget parent
                let lefts = takeWhile (/= ref) inputs
                spans  <- mapM readCodeSpan lefts
                let leftSpan = mconcat spans
                return leftSpan
            _ -> error "something is no yes"

readRange :: ASTOp m => NodeRef -> m (Int, Int)
readRange ref = do
    LeftSpacedSpan off  len  <- readRange' ref
    LeftSpacedSpan off' len' <- readCodeSpan ref
    let left' = fromIntegral off + fromIntegral len + fromIntegral off'
        left  = left' + (length ("def main:" :: String))
    return (left, left + fromIntegral len')

readCodeSpan :: ASTOp m => NodeRef -> m (LeftSpacedSpan Delta)
readCodeSpan ref = do
    maybecodespan  <- IR.getLayer @CodeSpan ref
    return $ fromMaybe (LeftSpacedSpan 0 0) maybecodespan

getNodeIdForMarker :: ASTOp m => Int -> m (Maybe NodeId)
getNodeIdForMarker index = do
    nodeSeq <- GraphBuilder.getNodeSeq
    case nodeSeq of
        Just nodeSeq -> do
            exprMap      <- IR.getLayer @CodeMarkers nodeSeq
            let exprMap' :: Map.Map Luna.Marker NodeRef
                exprMap' = coerce exprMap
                Just ref = Map.lookup (fromIntegral index) exprMap'
            varNodeId <- ASTRead.safeGetVarNodeId ref
            nodeId    <- ASTRead.getNodeId ref
            return $ varNodeId <|> nodeId
        _            -> return Nothing

markerCodeSpan :: GraphLocation -> Int -> Empire (Int, Int)
markerCodeSpan loc index = withGraph loc $ runASTOp $ do
    nodeSeq <- GraphBuilder.getNodeSeq
    case nodeSeq of
        Just nodeSeq -> do
            exprMap      <- IR.getLayer @CodeMarkers nodeSeq
            let exprMap' :: Map.Map Luna.Marker NodeRef
                exprMap' = coerce exprMap
                Just ref = Map.lookup (fromIntegral index) exprMap'
            readRange ref
        _            -> return (0,0)

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
    if disconnectOutputEdge
        then do
            updateNodeSequenceWithOutput (Just nothing)
            let item = BH.ExprItem Map.empty $ BH.AnonymousNode nothing
            uid <- liftIO $ UUID.nextRandom
            Graph.breadcrumbHierarchy . BH.children . at uid ?= BH.ExprChild item
            IR.putLayer @Marker nothing $ Just $ OutPortRef (NodeLoc def uid) []
        else GraphUtils.rewireNode dst nothing

unAcc :: ASTOp m => NodeId -> m ()
unAcc nodeId = do
    dstAst     <- GraphUtils.getASTTarget   nodeId
    newNodeRef <- ASTBuilder.removeAccessor dstAst
    GraphUtils.rewireNode nodeId newNodeRef

unApp :: ASTOp m => NodeId -> Int -> m ()
unApp nodeId pos = do
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

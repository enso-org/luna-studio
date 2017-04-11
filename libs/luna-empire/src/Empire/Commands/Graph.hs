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
import           Empire.Prelude
import qualified Safe
import qualified System.IO                     as IO

import           Empire.Data.AST                 (NodeRef, NotInputEdgeException (..), NotUnifyException,
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
import           Empire.API.Data.Node            (Node (..), NodeId)
import qualified Empire.API.Data.Node            as Node
import           Empire.API.Data.NodeLoc         (NodeLoc (..))
import qualified Empire.API.Data.NodeLoc         as NodeLoc
import           Empire.API.Data.NodeMeta        (NodeMeta)
import qualified Empire.API.Data.NodeMeta        as NodeMeta
import           Empire.API.Data.Port            (InPort (..), OutPort (..), PortId (..))
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

import qualified Luna.IR            as IR
import qualified OCI.IR.Combinators as IR (replaceSource, deleteSubtree, narrowTerm)
import           Luna.Syntax.Text.Parser.Marker (MarkedExprMap(..))
import qualified Luna.Syntax.Text.Parser.Marker as Luna
import qualified Luna.Syntax.Text.Parser.Parser as Parser (ReparsingStatus(..), ReparsingChange(..))


generateNodeName :: ASTOp m => m String
generateNodeName = do
    lastNameId <- use Graph.lastNameId
    let newNameId = lastNameId + 1
    Graph.lastNameId .= newNameId
    return $ "node" <> show newNameId

addNodeCondTC :: Bool -> GraphLocation -> NodeId -> Text -> NodeMeta -> Empire Node
addNodeCondTC True  loc uuid expr meta = addNode loc uuid expr meta
addNodeCondTC False loc uuid expr meta = withGraph loc $ addNodeNoTC loc uuid expr meta

addNode :: GraphLocation -> NodeId -> Text -> NodeMeta -> Empire Node
addNode loc uuid expr meta = withTC loc False $ addNodeNoTC loc uuid expr meta

addNodeNoTC :: GraphLocation -> NodeId -> Text -> NodeMeta -> Command Graph Node
addNodeNoTC loc uuid input meta = do
    parse <- fst <$> ASTParse.runParser input
    expr <- runASTOp $ do
        newNodeName  <- generateNodeName
        parsedNode   <- AST.addNode uuid newNodeName parse
        let nodeItem = BH.ExprItem Map.empty (BH.MatchNode parsedNode)
        Graph.breadcrumbHierarchy . BH.children . at uuid ?= BH.ExprChild nodeItem
        return parsedNode
    runAliasAnalysis
    node <- runASTOp $ do
        parsedRef <- ASTRead.getASTTarget uuid
        lamItem   <- prepareLambdaChild (BH.MatchNode expr) parsedRef
        exprItem  <- prepareExprChild   (BH.MatchNode expr) parsedRef
        forM_ lamItem  $ (Graph.breadcrumbHierarchy . BH.children . ix uuid .=) . BH.LambdaChild
        forM_ exprItem $ (Graph.breadcrumbHierarchy . BH.children . ix uuid .=) . BH.ExprChild
        AST.writeMeta expr meta
        updateNodeSequence
        GraphBuilder.buildNode uuid
    Publisher.notifyNodeUpdate loc node
    return node

prepareLambdaChild :: ASTOp m => BH.NodeIDTarget -> NodeRef -> m (Maybe BH.LamItem)
prepareLambdaChild tgt ref = do
    parsedIsLambda <- ASTRead.isLambda ref
    if parsedIsLambda then do
        lambdaUUID             <- liftIO $ UUID.nextRandom
        portMapping            <- liftIO $ (,) <$> UUID.nextRandom <*> UUID.nextRandom
        lambdaOutput           <- ASTRead.getLambdaOutputRef   ref
        lambdaBody             <- ASTRead.getFirstNonLambdaRef ref
        outputIsOneOfTheInputs <- AST.isTrivialLambda ref
        let anonOutput = if   not outputIsOneOfTheInputs
                         then Just $ BH.ExprChild $ BH.ExprItem Map.empty (BH.AnonymousNode lambdaOutput)
                         else Nothing
            lamItem    = BH.LamItem portMapping tgt (Map.empty & at lambdaUUID .~ anonOutput) lambdaBody
        IR.putLayer @Marker lambdaOutput $ Just $ OutPortRef (NodeLoc def lambdaUUID) Port.All
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

addPort :: GraphLocation -> NodeId -> Int -> Empire Node
addPort loc nid position = withGraph loc $ runASTOp $ do
    Just ref <- ASTRead.getCurrentASTTarget
    edges <- GraphBuilder.getEdgePortMapping
    when ((fst <$> edges) /= Just nid) $ throwM NotInputEdgeException
    ASTModify.addLambdaArg position ref
    -- TODO[MM]: This should match for any node. Now it ignores node and replace it by InputEdge.
    inputEdge <- GraphBuilder.buildConnections >>= \c -> GraphBuilder.buildInputEdge c nid
    return inputEdge

generateNodeId :: IO NodeId
generateNodeId = UUID.nextRandom

addSubgraph :: GraphLocation -> [Node] -> [Connection] -> Empire ([Node])
addSubgraph loc nodes conns = withTC loc False $ do
    newNodes <- fmap catMaybes $ forM nodes $ \n -> case n ^. Node.nodeType of
        Node.ExpressionNode expr -> Just <$> addNodeNoTC loc (n ^. Node.nodeId) expr (n ^. Node.nodeMeta)
        _ -> return Nothing
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

removePort :: GraphLocation -> AnyPortRef -> Empire Node
removePort loc portRef = withGraph loc $ runASTOp $ do
    let nodeId = portRef ^. PortRef.nodeId
    Just ref    <- ASTRead.getCurrentASTTarget
    edges <- GraphBuilder.getEdgePortMapping
    newRef <- case edges of
        Just (input, _output) -> do
            if nodeId == input then ASTModify.removeLambdaArg (portRef ^. PortRef.portId) ref
                               else throwM NotInputEdgeException
        _ -> return ref
    when (ref /= newRef) $ ASTModify.rewireCurrentNode newRef
    GraphBuilder.buildConnections >>= \c -> GraphBuilder.buildInputEdge c nodeId

movePort :: GraphLocation -> AnyPortRef -> Int -> Empire Node
movePort loc portRef newPosition = withGraph loc $ runASTOp $ do
    let nodeId = portRef ^. PortRef.nodeId
    Just ref    <- ASTRead.getCurrentASTTarget
    edges       <- GraphBuilder.getEdgePortMapping
    newRef      <- case edges of
        Just (input, _) -> do
            if nodeId == input then ASTModify.moveLambdaArg (portRef ^. PortRef.portId) newPosition ref
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
            if nodeId == input then ASTModify.renameLambdaArg (portRef ^. PortRef.portId) newName ref
                               else throwM NotInputEdgeException
        _ -> throwM NotInputEdgeException
    GraphBuilder.buildConnections >>= \c -> GraphBuilder.buildInputEdge c nodeId

setNodeExpression :: GraphLocation -> NodeId -> Text -> Empire Node
setNodeExpression loc nodeId expr = withTC loc False $ do
    oldExpr    <- runASTOp $ ASTRead.getASTTarget nodeId
    parsedRef  <- view _1 <$> ASTParse.runReparser expr oldExpr
    runASTOp $ ASTModify.rewireNode nodeId parsedRef
    runAliasAnalysis
    node <- runASTOp $ do
        refNode        <- ASTRead.getASTPointer nodeId
        parsedIsLambda <- ASTRead.isLambda parsedRef
        when parsedIsLambda $ do
            lambdaUUID             <- liftIO $ UUID.nextRandom
            lambdaOutput           <- ASTRead.getLambdaOutputRef   parsedRef
            lambdaBody             <- ASTRead.getFirstNonLambdaRef parsedRef
            portMapping            <- liftIO $ (,) <$> UUID.nextRandom <*> UUID.nextRandom
            outputIsOneOfTheInputs <- AST.isTrivialLambda parsedRef
            let anonOutput = if   not outputIsOneOfTheInputs
                             then Just $ BH.ExprChild $ BH.ExprItem Map.empty (BH.AnonymousNode lambdaOutput)
                             else Nothing
                lamItem    = BH.LamItem portMapping (BH.MatchNode refNode) (Map.empty & at lambdaUUID .~ anonOutput) lambdaBody
            Graph.breadcrumbHierarchy . BH.children . at nodeId  ?= BH.LambdaChild lamItem
            IR.putLayer @Marker lambdaOutput $ Just $ OutPortRef (NodeLoc def lambdaUUID) Port.All
        updateNodeSequence
        GraphBuilder.buildNode nodeId
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

connectCondTC :: Bool -> GraphLocation -> OutPortRef -> AnyPortRef -> Empire Connection
connectCondTC True  loc outPort anyPort = connect loc outPort anyPort
connectCondTC False loc outPort anyPort = withGraph loc $ connectNoTC loc outPort anyPort

connect :: GraphLocation -> OutPortRef -> AnyPortRef -> Empire Connection
connect loc outPort anyPort = withTC loc False $ connectNoTC loc outPort anyPort

connectPersistent :: ASTOp m => OutPortRef -> AnyPortRef -> m Connection
connectPersistent src@(OutPortRef (NodeLoc _ srcNodeId) srcPort) (InPortRef' dst@(InPortRef (NodeLoc _ dstNodeId) dstPort)) = do
    let inputPos = case srcPort of
            All              -> 0   -- FIXME: do not equalise All with Projection 0
            Projection int _ -> int
    isPatternMatch <- ASTRead.nodeIsPatternMatch srcNodeId
    if isPatternMatch then do
        case dstPort of
            Self    -> $notImplemented
            Arg num -> connectToMatchedVariable src dst num
                      else do
        case dstPort of
            Self    -> makeAcc srcNodeId dstNodeId inputPos
            Arg num -> makeApp srcNodeId dstNodeId num inputPos
    return $ Connection src dst
connectPersistent src@(OutPortRef (NodeLoc _ srcNodeId) srcPort) (OutPortRef' dst@(OutPortRef (NodeLoc _ dstNodeId) dstPort)) = do
    case dstPort of
        All            -> connectToPattern srcNodeId dstNodeId
        Projection _ _ -> $notImplemented

data CannotConnectException = CannotConnectException NodeId NodeId
    deriving Show

instance Exception CannotConnectException where
    toException = astExceptionToException
    fromException = astExceptionFromException

transplantMarker :: ASTOp m => NodeRef -> NodeRef -> m ()
transplantMarker donor recipient = do
    marker     <- IR.getLayer @Marker donor
    varsInside <- ASTRead.getVarsInside recipient
    let indexedVars = zip varsInside [0..]

        markerPort (Just (OutPortRef nid _)) index = Just (OutPortRef nid (Projection index All))
        markerPort _                         _     = Nothing
    forM_ indexedVars $ \(var, index) -> do
        IR.putLayer @Marker var $ markerPort marker index

data PatternLocation = Var | Target | Nowhere

getPatternLocation :: ASTOp m => NodeId -> m PatternLocation
getPatternLocation node = do
    varIsCons    <- do
      var    <- ASTRead.getASTVar node
      cons   <- IR.narrowTerm @IR.Cons var
      return $ isJust cons
    targetIsCons <- do
        target <- ASTRead.getASTTarget node
        cons   <- IR.narrowTerm @IR.Cons target
        return $ isJust cons
    if | varIsCons    -> return Var
       | targetIsCons -> return Target
       | otherwise    -> return Nowhere

data NotAPatternException = NotAPatternException NodeRef
    deriving Show

instance Exception NotAPatternException where
    fromException = astExceptionFromException
    toException   = astExceptionToException

constructorToPattern :: ASTOp m => NodeRef -> m NodeRef
constructorToPattern cons = IR.matchExpr cons $ \case
    IR.App{}    -> do
        (f, args) <- ASTDeconstruct.deconstructApp cons
        fCons     <- IR.narrowTerm @IR.Cons f
        case fCons of
            Nothing -> throwM $ NotAPatternException cons
            Just c  -> do
                patterns       <- mapM constructorToPattern args
                constructor    <- IR.matchExpr c $ \case
                    IR.Cons n _ -> pure n
                newConstructor <- IR.cons constructor patterns
                return $ IR.generalize newConstructor

    IR.Var{}       -> return cons
    IR.Cons n args -> do
        patterns <- mapM (constructorToPattern <=< IR.source) args
        IR.generalize <$> IR.cons n patterns
    IR.Number{}    -> return cons
    IR.String{}    -> return cons
    IR.Grouped g   -> constructorToPattern =<< IR.source g

data PatternConversion = ConvertToPattern | Don'tConvertToPattern

movePatternToVar :: ASTOp m => PatternConversion -> NodeId -> m ()
movePatternToVar convertPattern nid = do
    let converter ConvertToPattern      = constructorToPattern
        converter Don'tConvertToPattern = return
    cons     <- ASTRead.getASTTarget nid >>= converter convertPattern
    consName <- ASTRead.getASTVar nid
    transplantMarker consName cons
    ASTModify.rewireNodeName nid cons

connectToPattern :: ASTOp m => NodeId -> NodeId -> m Connection
connectToPattern src dst = do
    patternLocation <- getPatternLocation dst
    case patternLocation of
        Nowhere -> movePatternToVar ConvertToPattern dst
        Target  -> movePatternToVar Don'tConvertToPattern dst
        Var     -> return () -- no additional action required

    value <- ASTRead.getASTVar src
    ASTModify.rewireNode dst value

    return $ Connection (OutPortRef (NodeLoc def src) Port.All) (InPortRef (NodeLoc def dst) (Port.Arg 0))

connectToMatchedVariable :: ASTOp m => OutPortRef -> InPortRef -> Int -> m ()
connectToMatchedVariable (OutPortRef (NodeLoc _ srcNodeId) srcPort) (InPortRef (NodeLoc _ dstNodeId) dstPort) pos = do
    pattern    <- ASTRead.getASTVar srcNodeId
    varsInside <- ASTRead.getVarsInside pattern
    let inputPos = case srcPort of
            All              -> 0   -- FIXME: do not equalise All with Projection 0
            Projection int _ -> int
    let varToConnectTo = varsInside `Safe.atMay` inputPos
    case varToConnectTo of
        Nothing  -> throwM $ CannotConnectException srcNodeId dstNodeId
        Just var -> do
            dstAst     <- ASTRead.getASTTarget dstNodeId
            newNodeRef <- ASTBuilder.applyFunction dstAst var pos
            GraphUtils.rewireNode dstNodeId newNodeRef

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

getPortDefault :: GraphLocation -> AnyPortRef -> Empire PortDefault
getPortDefault loc (OutPortRef' (OutPortRef (NodeLoc _ nodeId) _))      = withGraph loc $ runASTOp $ GraphBuilder.getDefault =<< GraphUtils.getASTTarget nodeId
getPortDefault loc (InPortRef'  (InPortRef  _ Self))        = throwError "Cannot set default value on self port"
getPortDefault loc (InPortRef'  (InPortRef  (NodeLoc _ nodeId) (Arg x))) = withGraph loc $ runASTOp $ flip GraphBuilder.getInPortDefault x =<< GraphUtils.getASTTarget nodeId

setPortDefault :: GraphLocation -> AnyPortRef -> PortDefault -> Empire ()
setPortDefault loc portRef val = withTC loc False $ runASTOp $ setPortDefault' portRef val

setPortDefault' :: ASTOp m => AnyPortRef -> PortDefault -> m ()
setPortDefault' portRef val = do
    parsed <- ASTParse.parsePortDefault val
    (nodeId, newRef) <- case portRef of
        InPortRef' (InPortRef (NodeLoc _ nodeId) port) -> do
            ref <- GraphUtils.getASTTarget nodeId
            newRef <- case port of
                Self    -> ASTBuilder.makeAccessor parsed ref
                Arg num -> ASTBuilder.applyFunction ref parsed num
            return (nodeId, newRef)
        OutPortRef' (OutPortRef (NodeLoc _ nodeId) _) -> return (nodeId, parsed)
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

getNodes :: GraphLocation -> Empire [Node]
getNodes loc = withTC loc True $ runASTOp $ view APIGraph.nodes <$> GraphBuilder.buildGraph

getConnections :: GraphLocation -> Empire [(OutPortRef, InPortRef)]
getConnections loc = withTC loc True $ runASTOp $ view APIGraph.connections <$> GraphBuilder.buildGraph

decodeLocation :: GraphLocation -> Empire (Breadcrumb (Named BreadcrumbItem))
decodeLocation loc@(GraphLocation _ crumbs) = withGraph loc $ GraphBuilder.decodeBreadcrumbs crumbs

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
                    void $ insertNode Nothing expr
                Parser.ChangedExpr oldExpr expr -> do
                    oldNodeId <- (ASTRead.getVarNode oldExpr >>= ASTRead.getNodeId) `catch`
                        (\(_e :: NotUnifyException) -> return Nothing)

                    forM_ oldNodeId $ \nodeId -> do
                        oldMeta <- AST.readMeta oldExpr
                        forM oldMeta $ AST.writeMeta expr
                        varNode <- ASTRead.getVarNode expr
                        IR.putLayer @Marker varNode (Just $ OutPortRef (NodeLoc def nodeId) Port.All)
                        let nodeItem = BH.ExprItem Map.empty (BH.MatchNode expr)
                        Graph.breadcrumbHierarchy . BH.children . at nodeId ?= BH.ExprChild nodeItem
                        ASTModify.rewireNode nodeId =<< ASTRead.getTargetNode expr
                Parser.UnchangedExpr oldExpr expr -> do
                    oldMeta <- AST.readMeta oldExpr
                    forM oldMeta $ AST.writeMeta expr
                    Just oldNodeId <- (ASTRead.getVarNode oldExpr >>= ASTRead.getNodeId) `catch`
                        (\(_e :: NotUnifyException) -> return Nothing)
                    var <- ASTRead.getVarNode expr
                    IR.putLayer @Marker var (Just $ OutPortRef (NodeLoc def oldNodeId) Port.All)
                    let nodeItem = BH.ExprItem Map.empty (BH.MatchNode expr)
                    Graph.breadcrumbHierarchy . BH.children . at oldNodeId ?= BH.ExprChild nodeItem
                Parser.RemovedExpr oldExpr -> do
                    oldNodeId <- (ASTRead.getVarNode oldExpr >>= ASTRead.getNodeId) `catch`
                        (\(_e :: NotUnifyException) -> return Nothing)
                    forM_ oldNodeId $ removeNodeNoTC
            runAliasAnalysis
            return $ Just rs
        Nothing -> return Nothing

insertNode :: ASTOp m => Maybe NodeId -> NodeRef -> m NodeId
insertNode oldUUID expr = do
    newUUID <- liftIO $ UUID.nextRandom
    let uuid = fromMaybe newUUID oldUUID
    assignment <- ASTRead.isMatch expr
    if assignment then do
        var <- ASTRead.getVarNode expr
        IR.putLayer @Marker var (Just $ OutPortRef (NodeLoc def uuid) Port.All)
        let nodeItem = BH.ExprItem Map.empty (BH.MatchNode expr)
        Graph.breadcrumbHierarchy . BH.children . at uuid ?= BH.ExprChild nodeItem

        target   <- ASTRead.getASTTarget uuid
        lamItem  <- prepareLambdaChild (BH.MatchNode expr) target
        exprItem <- prepareExprChild   (BH.MatchNode expr) target
        forM_ lamItem  $ (Graph.breadcrumbHierarchy . BH.children . ix uuid .=) . BH.LambdaChild
        forM_ exprItem $ (Graph.breadcrumbHierarchy . BH.children . ix uuid .=) . BH.ExprChild
    else do
        let nodeItem = BH.ExprItem Map.empty (BH.AnonymousNode expr)
        Graph.breadcrumbHierarchy . BH.children . at uuid ?= BH.ExprChild nodeItem
    return uuid

loadCode :: Text -> Command Graph ()
loadCode code = do
    (ref, exprMap) <- ASTParse.runUnitParser code
    runASTOp $ forM_ (coerce exprMap :: Map.Map Luna.Marker NodeRef) $ insertNode Nothing
    Graph.breadcrumbHierarchy . BH._ToplevelParent . BH.topBody .= Just ref
    Graph.breadcrumbHierarchy . BH.body .= ref
    runAliasAnalysis

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
disconnectPort (InPortRef (NodeLoc _ dstNodeId) dstPort) = do
    isPatternMatch <- ASTRead.nodeIsPatternMatch dstNodeId
    if isPatternMatch then do
        nothing <- IR.generalize <$> IR.cons_ "Nothing"
        ASTModify.rewireNode dstNodeId nothing
                      else do
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

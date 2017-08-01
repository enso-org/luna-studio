{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE ViewPatterns          #-}

module Empire.Commands.Graph
    ( addNode
    , addNodeCondTC
    , addPort
    , addPortWithConnections
    , addSubgraph
    , autolayout
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
    , substituteCodeFromPoints
    , loadCode
    , markerCodeSpan
    , readCodeSpan
    , getNodeIdForMarker
    , updateCodeSpan
    , withTC
    , withGraph
    , withGraph'
    , withUnit
    , runTC
    , getName
    , MarkerNodeMeta(..)
    , FileMetadata(..)
    , dumpMetadata
    , addMetadataToCode
    , readMetadata
    , prepareCopy
    , paste
    , collapseToFunction
    , moveToOrigin
    ) where

import           Control.Arrow                    ((&&&))
import           Control.Monad                    (forM, forM_)
import           Control.Monad.Catch              (handle, onException)
import           Control.Monad.State              hiding (when)
import           Data.Aeson                       (FromJSON, ToJSON)
import qualified Data.Aeson                       as Aeson
import qualified Data.Aeson.Text                  as Aeson
import           Data.Coerce                      (coerce)
import           Data.Char                        (isSeparator)
import           Data.Foldable                    (toList)
import           Data.List                        (elemIndex, find, group, partition, sortOn, nub)
import qualified Data.List.Split                  as Split
import           Data.Map                         (Map)
import qualified Data.Map                         as Map
import           Data.Maybe                       (fromMaybe, maybeToList)
import qualified Data.Set                         as Set
import           Data.Set                         (Set)
import           Data.Text                        (Text)
import           Data.Text.Strict.Lens            (packed)
import qualified Data.Text                        as Text
import qualified Data.Text.Encoding               as Text
import qualified Data.Text.Lazy                   as TL
import qualified Data.Text.IO                     as Text
import           Data.Text.Position               (Delta)
import           Data.Text.Span                   (LeftSpacedSpan (..), SpacedSpan (..), leftSpacedSpan)
import qualified Data.UUID.V4                     as UUID (nextRandom)
import           Debug
import           Empire.ASTOp                     (ClassOp, GraphOp, putNewIR, putNewIRCls, runASTOp, runAliasAnalysis)
import qualified Empire.ASTOps.Builder            as ASTBuilder
import qualified Empire.ASTOps.Deconstruct        as ASTDeconstruct
import           Empire.ASTOps.BreadcrumbHierarchy (getMarker, prepareChild, makeTopBreadcrumbHierarchy)
import qualified Empire.ASTOps.Modify             as ASTModify
import           Empire.ASTOps.Parse              (FunctionParsing(..))
import qualified Empire.ASTOps.Parse              as ASTParse
import qualified Empire.ASTOps.Print              as ASTPrint
import qualified Empire.ASTOps.Read               as ASTRead
import qualified Empire.Commands.AST              as AST
import qualified Empire.Commands.Autolayout       as Autolayout
import           Empire.Commands.Breadcrumb       (makeGraph, makeGraphCls, withBreadcrumb)
import           Empire.Commands.Code             (addExprMapping, getNextExprMarker, getExprMap, setExprMap)
import qualified Empire.Commands.Code             as Code
import qualified Empire.Commands.GraphBuilder     as GraphBuilder
import qualified Empire.Commands.GraphUtils       as GraphUtils
import qualified Empire.Commands.Library          as Library
import qualified Empire.Commands.Publisher        as Publisher
import           Empire.Data.AST                  (InvalidConnectionException (..), EdgeRef, NodeRef, NotInputEdgeException (..),
                                                   SomeASTException, astExceptionFromException, astExceptionToException)
import qualified Empire.Data.BreadcrumbHierarchy  as BH
import           Empire.Data.Graph                (ClsGraph, Graph, NodeCache(..), portMappingMap, nodeIdMap)
import qualified Empire.Data.Graph                as Graph
import           Empire.Data.Layers               (Marker, SpanLength, SpanOffset)
import qualified Empire.Data.Library              as Library
import           Empire.Empire
import           Empire.Prelude                   hiding (toList)
import qualified Luna.IR                          as IR
import qualified Luna.IR.Term.Core                as Term
import qualified Luna.Syntax.Text.Lexer.Name      as Lexer
import           Luna.Syntax.Text.Parser.CodeSpan (CodeSpan)
import qualified Luna.Syntax.Text.Parser.CodeSpan as CodeSpan
import           Luna.Syntax.Text.Parser.Marker   (MarkedExprMap (..))
import qualified Luna.Syntax.Text.Parser.Marker   as Luna
import           LunaStudio.API.JSONInstances     ()
import           LunaStudio.Data.Breadcrumb       (Breadcrumb (..), BreadcrumbItem, Named)
import qualified LunaStudio.Data.Breadcrumb       as Breadcrumb
import           LunaStudio.Data.Constants        (gapBetweenNodes)
import           LunaStudio.Data.Connection       (Connection (..))
import qualified LunaStudio.Data.Graph            as APIGraph
import           LunaStudio.Data.GraphLocation    (GraphLocation (..))
import           LunaStudio.Data.Node             (ExpressionNode (..), InputSidebar (..), NodeId)
import qualified LunaStudio.Data.Node             as Node
import           LunaStudio.Data.NodeLoc          (NodeLoc (..))
import qualified LunaStudio.Data.NodeLoc          as NodeLoc
import           LunaStudio.Data.NodeMeta         (NodeMeta)
import qualified LunaStudio.Data.NodeMeta         as NodeMeta
import           LunaStudio.Data.Point            (Point)
import           LunaStudio.Data.Port             (InPortId, InPortIndex (..), OutPortId, getPortNumber)
import           LunaStudio.Data.PortDefault      (PortDefault)
import           LunaStudio.Data.PortRef          (AnyPortRef (..), InPortRef (..), OutPortRef (..))
import qualified LunaStudio.Data.PortRef          as PortRef
import           LunaStudio.Data.Position         (Position)
import qualified LunaStudio.Data.Position         as Position
import qualified OCI.IR.Combinators               as IR (replaceSource, deleteSubtree, narrow, narrowTerm, replace)
import qualified Safe

addNode :: GraphLocation -> NodeId -> Text -> NodeMeta -> Empire ExpressionNode
addNode = addNodeCondTC True

addNodeCondTC :: Bool -> GraphLocation -> NodeId -> Text -> NodeMeta -> Empire ExpressionNode
addNodeCondTC tc loc@(GraphLocation f _) uuid expr meta
    | GraphLocation _ (Breadcrumb []) <- loc = do
        node <- addFunNode loc AppendNone uuid expr meta
        resendCode loc
        return node
    | otherwise = do
        let runner = if tc then withTC loc False else withGraph loc
        node <- runner $ addNodeNoTC loc uuid expr Nothing meta
        resendCode loc
        return node

findPreviousFunction :: ClassOp m => NodeMeta -> [NodeRef] -> m (Maybe NodeRef)
findPreviousFunction newMeta functions = do
    functionWithPositions <- forM functions $ \fun -> do
        meta <- fromMaybe def <$> AST.readMeta fun
        return (fun, Position.toTuple $ meta ^. NodeMeta.position)
    let newMetaX = newMeta ^. NodeMeta.position . Position.x
        functionsToTheLeft = filter (\(f, (x, y)) -> x < newMetaX) functionWithPositions
        nearestNode = Safe.headMay $ reverse $ sortOn (view $ _2 . _1) functionsToTheLeft
    return $ fmap (view _1) nearestNode

putNewFunctionRef :: ClassOp m => EdgeRef -> Maybe NodeRef -> [EdgeRef] -> m [EdgeRef]
putNewFunctionRef newFunction Nothing                    functions  = return (newFunction : functions)
putNewFunctionRef newFunction (Just _)                   []         = return [newFunction]
putNewFunctionRef newFunction pf@(Just previousFunction) (fun:funs) = do
    fun' <- IR.source fun
    if fun' == previousFunction
        then return (fun:newFunction:funs)
        else putNewFunctionRef newFunction pf funs >>= return . (fun:)

insertFunAfter :: ClassOp m => Maybe NodeRef -> NodeRef -> Text -> m Int
insertFunAfter previousFunction function code = do
    let defaultFunSpace = 2
    case previousFunction of
        Nothing -> do
            unit <- use Graph.clsClass
            funs <- ASTRead.classFunctions unit
            let firstFunction = Safe.headMay funs
            funBlockStart <- case firstFunction of
                Just fun -> Code.functionBlockStartRef fun
                _        -> Code.functionBlockStartRef =<< IR.matchExpr unit (\case
                    IR.Unit _ _ cls -> IR.source cls)
            (off, off') <- case firstFunction of
                Just firstFun -> do
                    LeftSpacedSpan (SpacedSpan off len) <- view CodeSpan.realSpan <$> IR.getLayer @CodeSpan firstFun
                    off' <- if (off /= 0) then return off else do
                        IR.putLayer @CodeSpan firstFun $ CodeSpan.mkRealSpan (LeftSpacedSpan (SpacedSpan defaultFunSpace len))
                        return defaultFunSpace
                    return (off, off')
                Nothing     -> return (defaultFunSpace, 0)
            let indentedCode = (if (isNothing firstFunction && funBlockStart /= 0)
                                then Text.replicate (fromIntegral off) "\n"
                                else "")
                             <> code
                             <> Text.replicate (fromIntegral off') "\n"
            Code.insertAt funBlockStart indentedCode
            LeftSpacedSpan (SpacedSpan _ funLen) <- view CodeSpan.realSpan <$> IR.getLayer @CodeSpan function
            IR.putLayer @CodeSpan function $ CodeSpan.mkRealSpan (LeftSpacedSpan (SpacedSpan off funLen))
            return $ Text.length indentedCode
        Just pf -> do
            funBlockStart <- Code.functionBlockStartRef pf
            LeftSpacedSpan (SpacedSpan off len) <- view CodeSpan.realSpan <$> IR.getLayer @CodeSpan pf
            let indentedCode = Text.replicate (fromIntegral defaultFunSpace) "\n" <> code
            Code.insertAt (funBlockStart+len) indentedCode
            LeftSpacedSpan (SpacedSpan _ funLen) <- view CodeSpan.realSpan <$> IR.getLayer @CodeSpan function
            IR.putLayer @CodeSpan function $ CodeSpan.mkRealSpan (LeftSpacedSpan (SpacedSpan defaultFunSpace funLen))
            return $ Text.length indentedCode

addFunNode :: GraphLocation -> FunctionParsing -> NodeId -> Text -> NodeMeta -> Empire ExpressionNode
addFunNode loc parsing uuid expr meta = withUnit loc $ do
    (parse, code) <- ASTParse.runFunHackParser expr parsing
    when (meta /= def) $ runASTOp $ AST.writeMeta parse meta
    name <- runASTOp $ IR.matchExpr parse $ \case
        IR.ASGRootedFunction n _ -> do
            name <- ASTRead.getVarName' =<< IR.source n
            return $ nameToString name
    klass <- use Graph.clsClass
    (insertedCharacters, codePosition) <- runASTOp $ do
        funs <- ASTRead.classFunctions klass
        previousFunction <- findPreviousFunction meta funs

        insertedCharacters <- insertFunAfter previousFunction parse code
        IR.matchExpr klass $ \case
            IR.Unit _ _ cls -> do
                cls' <- IR.source cls
                Just (cls'' :: IR.Expr (IR.ClsASG)) <- IR.narrow cls'
                l <- IR.unsafeGeneralize <$> IR.link parse cls''
                links <- IR.matchExpr cls' $ \case
                    IR.ClsASG _ _ _ decls -> return decls
                newFuns <- putNewFunctionRef l previousFunction links
                IR.modifyExprTerm cls'' $ wrapped . IR.termClsASG_decls .~ (map IR.unsafeGeneralize newFuns :: [IR.Link (IR.Expr IR.Draft) (IR.Expr Term.ClsASG)])
        codePosition       <- Code.functionBlockStartRef parse

        return (fromIntegral insertedCharacters, codePosition)

    Graph.clsFuns . traverse . _2 . Graph.fileOffset %= (\off -> if off >= codePosition then off + insertedCharacters else off)
    (uuid', graph) <- makeGraphCls parse (Just uuid)

    runASTOp $ GraphBuilder.buildClassNode uuid' name

addNodeNoTC :: GraphLocation -> NodeId -> Text -> Maybe Text -> NodeMeta -> Command Graph ExpressionNode
addNodeNoTC loc uuid input name meta = do
    let propInput = Text.strip input
    parse <- fst <$> ASTParse.runParser propInput
    expr <- runASTOp $ do
        Code.propagateLengths parse
        (parsedNode, newName) <- AST.addNode uuid name (generateNodeName parse) parse
        index        <- getNextExprMarker
        marker       <- IR.marker' index
        IR.putLayer @SpanLength marker $ convert $ Text.length $ Code.makeMarker index
        markedNode   <- IR.marked' marker parsedNode
        [l, r]       <- IR.inputs markedNode
        IR.putLayer @SpanOffset l 0
        IR.putLayer @SpanOffset r 0
        addExprMapping index markedNode
        let textExpr = Code.makeMarker index <> maybe mempty (<> " = ") newName <> propInput
        let nodeSpan = fromIntegral $ Text.length textExpr
        IR.putLayer @SpanLength markedNode nodeSpan
        putInSequence markedNode textExpr meta
        putIntoHierarchy uuid markedNode
        return markedNode
    runAliasAnalysis
    node <- runASTOp $ do
        putChildrenIntoHierarchy uuid expr
        AST.writeMeta expr meta
        node <- GraphBuilder.buildNode uuid
        return node
    return node

findPreviousSeq :: GraphOp m => NodeRef -> Set.Set NodeRef -> m (Maybe NodeRef)
findPreviousSeq seq nodesToTheLeft = IR.matchExpr seq $ \case
    IR.Seq l r -> do
        l' <- IR.source l
        r' <- IR.source r
        if Set.member r' nodesToTheLeft then return (Just r') else findPreviousSeq l' nodesToTheLeft
    _ -> return $ if Set.member seq nodesToTheLeft then Just seq else Nothing

findPreviousNodeInSequence :: GraphOp m => NodeRef -> NodeMeta -> [(NodeRef, NodeMeta)] -> m (Maybe NodeRef)
findPreviousNodeInSequence seq meta nodes = do
    let position           = Position.toTuple $ view NodeMeta.position meta
        nodesWithPositions = map (\(n, m) -> (n, Position.toTuple $ m ^. NodeMeta.position)) nodes
        nodesToTheLeft     = filter (\(n, (x, y)) -> x <= fst position) nodesWithPositions
    findPreviousSeq seq (Set.fromList $ map fst nodesToTheLeft)

findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM p [] = return Nothing
findM p (x:xs) = do
     b <- p x
     if b then return (Just x) else findM p xs

setSeqOffsets :: GraphOp m => NodeRef -> Delta -> Delta -> m ()
setSeqOffsets node loff roff = do
    [l, r] <- IR.inputs node
    IR.putLayer @SpanOffset l loff
    IR.putLayer @SpanOffset r roff

insertAfter :: GraphOp m => NodeRef -> Maybe NodeRef -> NodeRef -> Delta -> Text -> m (NodeRef, Bool)
insertAfter s after new textBeginning code = IR.matchExpr s $ \case
        IR.Seq l r -> do
            rt <- IR.source r
            if Just rt == after
                then do
                    indentBy <- Code.getCurrentIndentationLength
                    Code.insertAt (fromIntegral textBeginning) ("\n" <> Text.replicate (fromIntegral indentBy) " " <> code)
                    newSeq <- IR.generalize <$> IR.seq s new
                    setSeqOffsets newSeq 0 (indentBy + 1)
                    return (newSeq, True)
                else do
                    lt     <- IR.source l
                    rlen   <- IR.getLayer @SpanLength rt
                    roff   <- IR.getLayer @SpanOffset r
                    (res, shouldUpdate) <- insertAfter lt after new (textBeginning - rlen - roff) code
                    when shouldUpdate (IR.replaceSource res l)
                    return (res, False)
        _ -> do
            indentBy <- Code.getCurrentIndentationLength
            if after == Just s
                then do
                    Code.insertAt (fromIntegral textBeginning) ("\n" <> Text.replicate (fromIntegral indentBy) " " <> code)
                    newSeq <- IR.generalize <$> IR.seq s new
                    setSeqOffsets newSeq 0 (indentBy + 1)
                    return $ (newSeq, True)
                else do
                    slen <- IR.getLayer @SpanLength s
                    Code.insertAt (fromIntegral $ textBeginning - slen) (code <> "\n" <> Text.replicate (fromIntegral indentBy) " ")
                    newSeq <- IR.generalize <$> IR.seq new s
                    setSeqOffsets newSeq 0 (indentBy + 1)
                    return $ (newSeq, True)


putInSequence :: GraphOp m => NodeRef -> Text -> NodeMeta -> m ()
putInSequence ref code meta = do
    oldSeq             <- use $ Graph.breadcrumbHierarchy . BH.body
    nodes              <- AST.readSeq oldSeq
    nodesAndMetas      <- mapM (\n -> (n,) <$> AST.readMeta n) nodes
    let nodesWithMetas =  mapMaybe (\(n,m) -> (n,) <$> m) nodesAndMetas
    nearestNode        <- findPreviousNodeInSequence oldSeq meta nodesWithMetas
    blockEnd           <- Code.getCurrentBlockEnd
    (newS, shouldUpdate) <- insertAfter oldSeq nearestNode ref blockEnd code
    when shouldUpdate (updateGraphSeq $ Just newS)
    Code.gossipLengthsChanged newS

addOutputAtEnd :: GraphOp m => NodeRef -> NodeRef -> Delta -> m NodeRef
addOutputAtEnd initial out blockEnd = do
    indentBy <- Code.getCurrentIndentationLength
    code     <- ASTPrint.printFullExpression out
    seq      <- IR.generalize <$> IR.seq initial out
    len      <- IR.getLayer @SpanLength initial
    outLen   <- IR.getLayer @SpanLength out
    let offset = indentBy + 1
    IR.putLayer @SpanLength seq $ len + offset + outLen
    Code.insertAt blockEnd ("\n" <> Text.replicate (fromIntegral indentBy) " " <> code)
    setSeqOffsets seq 0 offset
    return seq

reconnectOut :: GraphOp m => NodeRef -> NodeRef -> Delta -> m (Maybe NodeRef)
reconnectOut seq out blockEnd = IR.matchExpr seq $ \case
    IR.Seq l r -> do
        right <- IR.source r
        IR.matchExpr right $ \case
            IR.Marked _ _ -> Just <$> addOutputAtEnd seq out blockEnd
            _             -> do
                len <- IR.getLayer @SpanLength right
                ASTBuilder.replaceEdgeSource r (blockEnd - len) out
                return Nothing
    IR.Marked _ _ -> Just <$> addOutputAtEnd seq out blockEnd
    _             -> do
        len  <- IR.getLayer @SpanLength seq
        code <- ASTPrint.printFullExpression out
        Code.applyDiff (blockEnd - len) blockEnd code
        return $ Just out

setOutputTo :: GraphOp m => NodeRef -> m ()
setOutputTo out = do
    oldSeq   <- use $ Graph.breadcrumbHierarchy . BH.body
    blockEnd <- Code.getCurrentBlockEnd
    newSeq   <- reconnectOut oldSeq out blockEnd
    mapM_ (updateGraphSeq . Just)   newSeq
    mapM_ Code.gossipUsesChanged newSeq

updateGraphSeq :: GraphOp m => Maybe NodeRef -> m ()
updateGraphSeq newOut = do
    oldSeq     <- use $ Graph.breadcrumbHierarchy . BH.body
    currentTgt <- ASTRead.getCurrentASTTarget
    outLink    <- ASTRead.getFirstNonLambdaLink currentTgt
    case (,) <$> outLink <*> newOut of
        Just (l, o) -> IR.replaceSource o l
        Nothing     -> return ()
    IR.deepDeleteWithWhitelist oldSeq $ Set.fromList $ maybeToList newOut
    oldRef <- use $ Graph.breadcrumbHierarchy . BH.self
    when (oldRef == oldSeq) $ forM_ newOut (Graph.breadcrumbHierarchy . BH.self .=)
    forM_ newOut $ (Graph.breadcrumbHierarchy . BH.body .=)

updateCodeSpan' :: GraphOp m => NodeRef -> m _
updateCodeSpan' ref = IR.matchExpr ref $ \case
    IR.Seq l r -> do
        l' <- updateCodeSpan' =<< IR.source l
        r' <- updateCodeSpan' =<< IR.source r
        let span = l' <> r'
        setCodeSpan ref span
        return span
    _ -> readCodeSpan ref

updateCodeSpan :: GraphOp m => NodeRef -> m ()
updateCodeSpan ref = do
    updateCodeSpan' ref
    LeftSpacedSpan (SpacedSpan off len) <- readCodeSpan ref
    fileOffset <- use Graph.fileOffset
    setCodeSpan ref (leftSpacedSpan fileOffset len)

addPort :: GraphLocation -> OutPortRef -> Empire InputSidebar
addPort loc portRef = withTC loc False $ addPortNoTC loc portRef

addPortNoTC :: GraphLocation -> OutPortRef -> Command Graph InputSidebar
addPortNoTC loc (OutPortRef nl pid) = runASTOp $ do
    let nid      = nl ^. NodeLoc.nodeId
        position = getPortNumber pid
    (inE, _) <- GraphBuilder.getEdgePortMapping
    when (inE /= nid) $ throwM NotInputEdgeException
    ref <- ASTRead.getCurrentASTTarget
    ASTBuilder.detachNodeMarkersForArgs ref
    ASTModify.addLambdaArg position ref
    newLam <- ASTRead.getCurrentASTTarget
    ASTBuilder.attachNodeMarkersForArgs nid [] newLam
    GraphBuilder.buildInputSidebar nid

addPortWithConnections :: GraphLocation -> OutPortRef -> [AnyPortRef] -> Empire InputSidebar
addPortWithConnections loc portRef connectTo = withTC loc False $ do
    newPorts <- addPortNoTC loc portRef
    forM_ connectTo $ connectNoTC loc portRef
    return newPorts

addSubgraph :: GraphLocation -> [ExpressionNode] -> [Connection] -> Empire [ExpressionNode]
addSubgraph loc@(GraphLocation _ (Breadcrumb [])) nodes _ = do
    res <- forM nodes $ \n -> addFunNode loc ParseAsIs (n ^. Node.nodeId) (n ^. Node.code) (n ^. Node.nodeMeta)
    resendCode loc
    return res
addSubgraph loc nodes conns = do
    newNodes <- withTC loc False $ do
        newNodes <- forM nodes $ \n -> addNodeNoTC loc (n ^. Node.nodeId) (n ^. Node.expression) (n ^. Node.name) (n ^. Node.nodeMeta)
        forM_ conns $ \(Connection src dst) -> connectNoTC loc src (InPortRef' dst)
        return newNodes
    resendCode loc
    return newNodes

removeNodes :: GraphLocation -> [NodeId] -> Empire ()
removeNodes loc@(GraphLocation file (Breadcrumb [])) nodeIds = do
    withUnit loc $ do
        funs <- use Graph.clsFuns
        let funsUUIDs = Map.fromList $ map (\(k, (n,g)) -> (k, n)) $ Map.assocs funs
            funsToRemove = catMaybes $ map (flip Map.lookup funsUUIDs) nodeIds

        let graphsToRemove = Map.elems $ Map.filterWithKey (\a _ -> a `elem` nodeIds) funs
        Graph.clsFuns .= Map.filterWithKey (\a _ -> a `notElem` nodeIds) funs

        klass <- use Graph.clsClass
        runASTOp $ IR.matchExpr klass $ \case
            IR.Unit _ _ cls -> do
                cls' <- IR.source cls
                Just (cls'' :: IR.Expr (IR.ClsASG)) <- IR.narrow cls'
                funs <- IR.matchExpr cls' $ \case
                    IR.ClsASG _ _ _ f -> do
                        links <- mapM (\link -> (link,) <$> IR.source link) f
                        forM links $ \(link, fun) -> do
                            IR.matchExpr fun $ \case
                                IR.ASGRootedFunction n _ -> do
                                    name <- ASTRead.getVarName' =<< IR.source n
                                    return $ if convert name `elem` funsToRemove then Left link else Right link
                                IR.Metadata{} -> return $ Right link
                let (toRemove, left) = partitionEithers funs
                spans <- forM toRemove $ \candidate -> do
                    ref <- IR.source candidate
                    start <- Code.functionBlockStartRef ref
                    LeftSpacedSpan (SpacedSpan off len) <- view CodeSpan.realSpan <$> IR.getLayer @CodeSpan.CodeSpan ref
                    return (start - off, start + len)
                forM (reverse spans) $ \(start, end) -> do
                    let removedCharacters = end - start
                    Graph.clsFuns . traverse . _2 . Graph.fileOffset %= (\off -> if off > end then off - removedCharacters else off)
                    Code.removeAt start end
                IR.modifyExprTerm cls'' $ wrapped . IR.termClsASG_decls .~ (map IR.unsafeGeneralize left)
                mapM (IR.deleteSubtree <=< IR.source) toRemove
    resendCode loc
removeNodes loc@(GraphLocation file _) nodeIds = do
    withTC loc False $ runASTOp $ mapM removeNodeNoTC nodeIds
    resendCode loc

deepRemoveExprMarkers :: GraphOp m => BH.BChild -> m ()
deepRemoveExprMarkers chld = do
    removeExprMarker $ chld ^. BH.self
    traverseOf_ (BH._LambdaChild . BH.children     . traverse) deepRemoveExprMarkers chld
    traverseOf_ (BH._ExprChild   . BH.portChildren . traverse . re BH._LambdaChild) deepRemoveExprMarkers chld

removeNodeNoTC :: GraphOp m => NodeId -> m [NodeId]
removeNodeNoTC nodeId = do
    astRef        <- ASTRead.getASTRef nodeId
    obsoleteEdges <- getOutEdges nodeId
    mapM_ disconnectPort obsoleteEdges
    mapM deepRemoveExprMarkers =<< use (Graph.breadcrumbHierarchy . BH.children . at nodeId)
    Graph.breadcrumbHierarchy . BH.children . at nodeId .= Nothing
    removeFromSequence astRef
    return $ map (view PortRef.dstNodeId) obsoleteEdges

removeExprMarker :: GraphOp m => NodeRef -> m ()
removeExprMarker ref = do
    exprMap <- getExprMap
    let newExprMap = Map.filter (/= ref) exprMap
    setExprMap newExprMap

removeSequenceElement :: GraphOp m => NodeRef -> NodeRef -> m (Maybe NodeRef, Bool)
removeSequenceElement seq ref = IR.matchExpr seq $ \case
    IR.Seq l r -> do
        rt <- IR.source r
        if rt == ref
            then do
                Just offset <- Code.getOffsetRelativeToFile ref
                len         <- IR.getLayer @SpanLength ref
                edgeOff     <- IR.getLayer @SpanOffset r
                Code.removeAt (offset - edgeOff) (offset + len)
                lt          <- IR.source l
                return (Just lt, True)
            else do
                lt    <- IR.source l
                recur <- removeSequenceElement lt ref
                case recur of
                    (Just newRef, True) -> do -- left child changed
                        IR.replace newRef lt
                        return (Just newRef, False)
                    (Nothing, True)     -> do -- left child removed, right child replaces whole seq
                        roff     <- IR.getLayer @SpanOffset r
                        loff     <- IR.getLayer @SpanOffset l
                        Just pos <- Code.getOffsetRelativeToFile ref
                        Code.removeAt pos (pos + roff + loff)
                        return (Just rt, True)
                    (res, False)        -> return (res, False)
    _ -> do
        Just offset <- Code.getOffsetRelativeToFile ref
        len         <- IR.getLayer @SpanLength ref
        Code.removeAt offset (offset + len)
        return (Nothing, True)

removeFromSequence :: GraphOp m => NodeRef -> m ()
removeFromSequence ref = do
    oldSeq <- use $ Graph.breadcrumbHierarchy . BH.body
    (newS, shouldUpdate) <- removeSequenceElement oldSeq ref
    when shouldUpdate (updateGraphSeq newS)
    mapM_ Code.gossipLengthsChanged newS

removePort :: GraphLocation -> OutPortRef -> Empire InputSidebar
removePort loc portRef = withGraph loc $ runASTOp $ do
    let nodeId = portRef ^. PortRef.srcNodeId
    ref <- ASTRead.getCurrentASTTarget
    ASTBuilder.detachNodeMarkersForArgs ref
    (inE, _) <- GraphBuilder.getEdgePortMapping
    newRef   <- if nodeId == inE then ASTModify.removeLambdaArg (portRef ^. PortRef.srcPortId) ref
                                 else throwM NotInputEdgeException
    when (ref /= newRef) $ ASTModify.rewireCurrentNode newRef
    ASTBuilder.attachNodeMarkersForArgs nodeId [] newRef
    GraphBuilder.buildInputSidebar nodeId

movePort :: GraphLocation -> OutPortRef -> Int -> Empire InputSidebar
movePort loc portRef newPosition = withGraph loc $ runASTOp $ do
    let nodeId = portRef ^. PortRef.srcNodeId
    ref        <- ASTRead.getCurrentASTTarget
    (input, _) <- GraphBuilder.getEdgePortMapping
    newRef     <- if nodeId == input then ASTModify.moveLambdaArg (portRef ^. PortRef.srcPortId) newPosition ref
                                     else throwM NotInputEdgeException
    when (ref /= newRef) $ ASTModify.rewireCurrentNode newRef
    ASTBuilder.attachNodeMarkersForArgs nodeId [] ref
    GraphBuilder.buildInputSidebar nodeId

renamePort :: GraphLocation -> OutPortRef -> Text -> Empire InputSidebar
renamePort loc portRef newName = withGraph loc $ runASTOp $ do
    let nodeId = portRef ^. PortRef.srcNodeId
    ref        <- ASTRead.getCurrentASTTarget
    (input, _) <- GraphBuilder.getEdgePortMapping
    if nodeId == input then ASTModify.renameLambdaArg (portRef ^. PortRef.srcPortId) (Text.unpack newName) ref
                       else throwM NotInputEdgeException
    GraphBuilder.buildInputSidebar nodeId

setNodeExpression :: GraphLocation -> NodeId -> Text -> Empire ExpressionNode
setNodeExpression loc@(GraphLocation file _) nodeId expr' = do
    let expression = Text.strip expr'
    node <- withTC loc False $ do
        oldExpr   <- runASTOp $ ASTRead.getASTTarget nodeId
        parsedRef <- view _1 <$> ASTParse.runReparser expression oldExpr
        (oldBeg, oldEnd) <- runASTOp $ do
            oldBegin  <- Code.getASTTargetBeginning nodeId
            oldTarget <- ASTRead.getASTTarget nodeId
            oldLen    <- IR.getLayer @SpanLength oldTarget
            Code.propagateLengths parsedRef
            ASTModify.rewireNode nodeId parsedRef
            return (oldBegin, oldBegin + oldLen)
        runAliasAnalysis
        node <- runASTOp $ do
            expr      <- ASTRead.getASTTarget nodeId
            marked    <- ASTRead.getASTRef nodeId
            item      <- prepareChild (NodeCache Map.empty Map.empty Map.empty) marked parsedRef
            Graph.breadcrumbHierarchy . BH.children . ix nodeId .= item
            let len = fromIntegral $ Text.length expression
            Code.applyDiff oldBeg oldEnd expression
            Code.gossipUsesChangedBy (len - (oldEnd - oldBeg)) expr
            node <- GraphBuilder.buildNode nodeId
            return node
        return node
    resendCode loc
    return node

updateExprMap :: GraphOp m => NodeRef -> NodeRef -> m ()
updateExprMap new old = do
    exprMap <- getExprMap
    let updated = Map.map (\a -> if a == old then new else a) exprMap
    setExprMap updated

resendCode :: GraphLocation -> Empire ()
resendCode loc@(GraphLocation file _) = do
    code <- fmap Text.pack $ getCode loc
    Publisher.notifyCodeUpdate file
                               (Code.deltaToPoint 0 code)
                               (Code.deltaToPoint (fromIntegral $ Text.length code) code)
                               code
                               Nothing

setNodeMetaGraph :: NodeId -> NodeMeta -> Command Graph ()
setNodeMetaGraph nodeId newMeta = runASTOp $ do
    ref <- ASTRead.getASTRef nodeId
    AST.writeMeta ref newMeta

setNodeMetaFun :: NodeId -> NodeMeta -> Command ClsGraph ()
setNodeMetaFun nodeId newMeta = runASTOp $ do
    Just (name, _) <- use $ Graph.clsFuns . at nodeId
    f              <- ASTRead.getFunByName name
    AST.writeMeta f newMeta

setNodeMeta :: GraphLocation -> NodeId -> NodeMeta -> Empire ()
setNodeMeta loc nodeId newMeta = withGraph' loc (setNodeMetaGraph nodeId newMeta) (setNodeMetaFun nodeId newMeta)

setNodePosition :: GraphLocation -> NodeId -> Position -> Empire ()
setNodePosition loc nodeId newPos = do
    oldMeta <- fromMaybe def <$> getNodeMeta loc nodeId
    setNodeMeta loc nodeId $ oldMeta & NodeMeta.position .~ newPos

setNodePositionAST :: GraphOp m => NodeId -> Position -> m ()
setNodePositionAST nodeId newPos = do
    ref <- ASTRead.getASTRef nodeId
    oldMeta <- fromMaybe def <$> AST.readMeta ref
    AST.writeMeta ref $ oldMeta & NodeMeta.position .~ newPos

setNodePositionCls :: ClassOp m => NodeId -> Position -> m ()
setNodePositionCls nodeId newPos = do
    Just (name, _) <- use $ Graph.clsFuns . at nodeId
    f              <- ASTRead.getFunByName name
    oldMeta <- fromMaybe def <$> AST.readMeta f
    AST.writeMeta f $ oldMeta & NodeMeta.position .~ newPos

connectCondTC :: Bool -> GraphLocation -> OutPortRef -> AnyPortRef -> Empire Connection
connectCondTC True  loc outPort anyPort = connect loc outPort anyPort
connectCondTC False loc outPort anyPort = do
    connection <- withGraph loc $ connectNoTC loc outPort anyPort
    resendCode loc
    return connection

connect :: GraphLocation -> OutPortRef -> AnyPortRef -> Empire Connection
connect loc outPort anyPort = do
    connection <- withTC loc False $ connectNoTC loc outPort anyPort
    resendCode loc
    return connection

connectPersistent :: GraphOp m => OutPortRef -> AnyPortRef -> m Connection
connectPersistent src@(OutPortRef (NodeLoc _ srcNodeId) srcPort) (InPortRef' dst@(InPortRef (NodeLoc _ dstNodeId) dstPort)) = do
    case dstPort of
        []        -> makeWhole srcNodeId dstNodeId srcPort
        _         -> makeInternalConnection srcNodeId dstNodeId srcPort dstPort
    return $ Connection src dst
connectPersistent src@(OutPortRef (NodeLoc _ srcNodeId) srcPort) (OutPortRef' dst@(OutPortRef d@(NodeLoc _ dstNodeId) dstPort)) = do
    case dstPort of
        []    -> do
            ASTBuilder.flipNode dstNodeId
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
    refBeg <- Code.getASTTargetBeginning nodeId
    e <- ASTRead.getTargetEdge nodeId
    case port of
        [Self]    -> do
            ASTBuilder.makeAccessor parsed e refBeg
        [Arg num] -> do
            ASTBuilder.applyFunction e refBeg parsed num
setPortDefault loc port Nothing = withTC loc False $ runASTOp $ disconnectPort port

disconnect :: GraphLocation -> InPortRef -> Empire ()
disconnect loc@(GraphLocation file _) port@(InPortRef (NodeLoc _ nid) _) = do
    withTC loc False $ runASTOp $ disconnectPort port
    resendCode loc

getNodeMeta :: GraphLocation -> NodeId -> Empire (Maybe NodeMeta)
getNodeMeta loc = withGraph loc . runASTOp . AST.getNodeMeta

getCode :: GraphLocation -> Empire String
getCode loc@(GraphLocation file _) = do
    code <- withUnit (GraphLocation file (Breadcrumb [])) $ runASTOp $ do
        unit     <- use Graph.clsClass
        metaRef  <- ASTRead.getMetadataRef unit
        case metaRef of
            Just meta -> do
                metaStart <- Code.functionBlockStartRef meta
                LeftSpacedSpan (SpacedSpan off len) <- view CodeSpan.realSpan <$> IR.getLayer @CodeSpan meta
                Code.getAt 0 metaStart
            _         ->
                use Graph.code
    return $ Text.unpack . Code.removeMarkers $ code

-- TODO[MK]: handle span
getBuffer :: FilePath -> Maybe (Int, Int) -> Empire Text
getBuffer file span = Text.pack <$> getCode (GraphLocation file (Breadcrumb []))

getGraphCondTC :: Bool -> GraphLocation -> Empire APIGraph.Graph
getGraphCondTC tc loc = (if tc then withTC' loc True else withGraph' loc) (runASTOp $ do
    exc <- use Graph.parseError
    case exc of
        Just e  -> throwM e
        Nothing -> GraphBuilder.buildGraph)
    (runASTOp $ do
        exc <- use Graph.clsParseError
        case exc of
            Just e  -> throwM e
            Nothing -> GraphBuilder.buildClassGraph)

getGraph :: GraphLocation -> Empire APIGraph.Graph
getGraph = getGraphCondTC True

getGraphNoTC :: GraphLocation -> Empire APIGraph.Graph
getGraphNoTC = getGraphCondTC False

getNodes :: GraphLocation -> Empire [ExpressionNode]
getNodes loc = withTC' loc True (runASTOp (view APIGraph.nodes <$> GraphBuilder.buildGraph))
                                (runASTOp (view APIGraph.nodes <$> GraphBuilder.buildClassGraph))

getConnections :: GraphLocation -> Empire [(OutPortRef, InPortRef)]
getConnections loc = withTC loc True $ runASTOp $ view APIGraph.connections <$> GraphBuilder.buildGraph

decodeLocation :: GraphLocation -> Empire (Breadcrumb (Named BreadcrumbItem))
decodeLocation loc@(GraphLocation file crumbs) = case crumbs of
        Breadcrumb [] -> return $ Breadcrumb []
        _             -> do
            definitionsIDs <- withUnit (GraphLocation file (Breadcrumb [])) $ do
                funs <- use Graph.clsFuns
                return $ Map.map fst funs
            withGraph (functionLocation loc) $ GraphBuilder.decodeBreadcrumbs definitionsIDs crumbs

renameNode :: GraphLocation -> NodeId -> Text -> Empire ()
renameNode loc nid name
    | GraphLocation _ (Breadcrumb []) <- loc = do
        withUnit loc $ do
            let stripped = Text.strip name
            _ <- liftIO $ ASTParse.runProperVarParser stripped
            oldName <- use $ Graph.clsFuns . ix nid . _1
            Graph.clsFuns %= Map.adjust (_1 .~ (Text.unpack stripped)) nid
            runASTOp $ do
                fun     <- ASTRead.getFunByName oldName
                IR.matchExpr fun $ \case
                    IR.ASGRootedFunction n _ -> flip ASTModify.renameVar (convert stripped) =<< IR.source n
                    _ -> return ()

                LeftSpacedSpan (SpacedSpan off oldLen) <- view CodeSpan.realSpan <$> IR.getLayer @CodeSpan fun
                let lengthDiff = fromIntegral (Text.length stripped - length oldName)
                    newLen = oldLen + lengthDiff
                IR.putLayer @CodeSpan fun $ CodeSpan.mkRealSpan (LeftSpacedSpan (SpacedSpan off newLen))

                Graph.clsFuns . ix nid . _2 . Graph.bodyOffset += lengthDiff
                g <- preuse (Graph.clsFuns . ix nid . _2) <?!> BH.BreadcrumbDoesNotExistException (Breadcrumb [Breadcrumb.Definition nid])
                let funOffset = g ^. Graph.fileOffset
                Graph.clsFuns . traverse . _2 . Graph.fileOffset %= (\off -> if off > funOffset then off + lengthDiff else off)

                functionStart <- Code.functionBlockStartRef fun
                let functionNameStart = functionStart + 4 -- "def "
                    functionNameEnd   = functionNameStart + fromIntegral (length oldName)
                Code.applyDiff functionNameStart functionNameEnd stripped
        resendCode loc
    | otherwise = do
        withTC loc False $ runASTOp $ do
            v <- ASTRead.getASTVar nid
            ASTModify.renameVar v $ convert name
            Code.replaceAllUses v name
        resendCode loc

dumpGraphViz :: GraphLocation -> Empire ()
dumpGraphViz loc = withGraph loc $ return ()

autolayoutNodes :: GraphOp m => [NodeId] -> m ()
autolayoutNodes nids = timeIt "autolayoutNodes" $ do
    nodes <- GraphBuilder.buildNodesForAutolayout <!!> "buildNodesForAutolayout"
    conns <- GraphBuilder.buildConnections        <!!> "buildConnections"
    let autolayout = Autolayout.autolayoutNodes nids nodes conns
    mapM_ (uncurry setNodePositionAST) autolayout <!!> "setNodePositionsAST"

openFile :: FilePath -> Empire ()
openFile path = do
    code <- liftIO (Text.readFile path) <!!> "readFile"
    Library.createLibrary Nothing path  <!!> "createLibrary"
    let loc = GraphLocation path $ Breadcrumb []
    result <- handle (\(e :: SomeASTException) -> return $ Left e) $ fmap Right $ do
        loadCode loc code <!!> "loadCode"
        withUnit loc $ Graph.clsParseError .= Nothing
    case result of
        Left e -> withUnit loc $ Graph.clsParseError ?= e
        _      -> return ()

typecheck :: GraphLocation -> Empire ()
typecheck loc = withTC' loc False (return ()) (return ())

substituteCodeFromPoints :: FilePath -> Point -> Point -> Text -> Maybe Point -> Empire ()
substituteCodeFromPoints path start end code cursor = do
    let loc = GraphLocation path (Breadcrumb [])
    (s, e) <- withUnit loc $ do
        oldCode   <- use Graph.code
        let noMarkers  = Code.removeMarkers oldCode
            deltas     = (Code.pointToDelta start noMarkers, Code.pointToDelta end noMarkers)
            realDeltas = Code.viewDeltasToReal oldCode deltas
        return realDeltas
    substituteCode path s e code Nothing

substituteCode :: FilePath -> Delta -> Delta -> Text -> Maybe Delta -> Empire ()
substituteCode path start end code cursor = do
    let loc = GraphLocation path (Breadcrumb [])
    newCode <- withUnit loc $ Code.applyDiff start end code
    handle (\(e :: SomeASTException) -> withUnit loc $ Graph.clsParseError ?= e) $ do
        withUnit loc $ Graph.clsParseError .= Nothing
        reloadCode loc newCode

lamItemToMapping :: ((NodeId, Maybe Int), BH.LamItem) -> ((NodeId, Maybe Int), (NodeId, NodeId))
lamItemToMapping (idArg, BH.LamItem portMapping _ _ _) = (idArg, portMapping)

extractMarkedMetasAndIds :: GraphOp m => NodeRef -> m [(Word64, (Maybe NodeMeta, Maybe NodeId))]
extractMarkedMetasAndIds root = IR.matchExpr root $ \case
    IR.Marked m e -> do
        meta   <- AST.readMeta root
        marker <- getMarker =<< IR.source m
        expr   <- IR.source e
        rest   <- extractMarkedMetasAndIds expr
        nid    <- ASTRead.getNodeId  expr
        return $ (marker, (meta, nid)) : rest
    _ -> concat <$> (mapM (extractMarkedMetasAndIds <=< IR.source) =<< IR.inputs root)

reloadCode :: GraphLocation -> Text -> Empire ()
reloadCode loc@(GraphLocation file _) code = do
    funs <- withUnit (GraphLocation file (Breadcrumb [])) $ do
        funs <- use Graph.clsFuns
        return $ Map.keys funs
    oldMetasAndIds <- forM funs $ \fun -> withGraph (GraphLocation file (Breadcrumb [Breadcrumb.Definition fun])) $ runASTOp $ do
        root       <- use $ Graph.breadcrumbHierarchy . BH.body
        oldMetas   <- extractMarkedMetasAndIds root
        return $ Map.fromList oldMetas
    previousPortMappings <- forM funs $ \fun -> withGraph (GraphLocation file (Breadcrumb [Breadcrumb.Definition fun])) $ runASTOp $ do
        hierarchy <- use Graph.breadcrumbHierarchy
        let lamItems = BH.getLamItems hierarchy
            elems    = map lamItemToMapping lamItems
        return $ Map.fromList elems
    let previousNodeIds   = Map.unions $ Map.mapMaybe snd <$> oldMetasAndIds
        previousNodeMetas = Map.unions $ Map.mapMaybe fst <$> oldMetasAndIds
    withUnit (GraphLocation file (Breadcrumb [])) $ Graph.nodeCache .= NodeCache previousNodeIds previousNodeMetas (Map.unions previousPortMappings)
    loadCode loc code

putIntoHierarchy :: GraphOp m => NodeId -> NodeRef -> m ()
putIntoHierarchy nodeId marked = do
    let nodeItem = BH.ExprItem Map.empty marked
    Graph.breadcrumbHierarchy . BH.children . at nodeId ?= BH.ExprChild nodeItem

putChildrenIntoHierarchy :: GraphOp m => NodeId -> NodeRef -> m ()
putChildrenIntoHierarchy uuid expr = do
    target       <- ASTRead.getASTTarget uuid
    marked       <- ASTRead.getASTRef uuid
    item         <- prepareChild (NodeCache Map.empty Map.empty Map.empty) marked target
    Graph.breadcrumbHierarchy . BH.children . ix uuid .= item

copyMeta :: GraphOp m => NodeRef -> NodeRef -> m ()
copyMeta donor recipient = do
    meta <- AST.readMeta donor
    forM_ meta $ AST.writeMeta recipient

markNode :: GraphOp m => NodeId -> m ()
markNode nodeId = do
    var <- ASTRead.getASTMarkerPosition nodeId
    ASTBuilder.attachNodeMarkers nodeId [] var

loadCode :: GraphLocation -> Text -> Empire ()
loadCode (GraphLocation file _) code = do
    let loc = GraphLocation file def
    (unit, IR.Rooted ir ref, exprMap) <- liftIO $ ASTParse.runProperParser code
    activeFiles . at file . traverse . Library.body . Graph.clsClass .= unit
    activeFiles . at file . traverse . Library.body . Graph.clsCodeMarkers .= (coerce exprMap)
    activeFiles . at file . traverse . Library.body . Graph.code .= code
    funs <- use $ activeFiles . at file . traverse . Library.body . Graph.clsFuns
    let funsUUIDs = Map.fromList $ map (\(k, (n,g)) -> (n, k)) $ Map.assocs funs
    activeFiles . at file . traverse . Library.body . Graph.clsFuns .= Map.empty
    withUnit (GraphLocation file (Breadcrumb [])) $ do
        putNewIRCls ir
        FileMetadata fileMetadata <- runASTOp $ readMetadata'
        let savedNodeMetas = Map.fromList $ map (\(MarkerNodeMeta m meta) -> (m, meta)) fileMetadata
        Graph.nodeCache . Graph.nodeMetaMap %= (\cache -> Map.union cache savedNodeMetas)
    functions <- withUnit loc $ do
        klass <- use Graph.clsClass
        runASTOp $ do
            funs <- ASTRead.classFunctions klass
            forM funs $ \f -> IR.matchExpr f $ \case
                IR.ASGRootedFunction n _ -> do
                    name <- ASTRead.getVarName' =<< IR.source n
                    return (convert name, f)
    forM_ functions $ \(name, fun) -> do
        let lastUUID = Map.lookup name funsUUIDs
        uuid <- Library.withLibrary file (fst <$> makeGraph fun lastUUID)
        let loc' = GraphLocation file $ Breadcrumb [Breadcrumb.Definition uuid]
        autolayout loc'
    autolayoutTopLevel loc
    return ()

infixl 5 |>
(|>) :: GraphLocation -> BreadcrumbItem -> GraphLocation
(|>) (GraphLocation file bc) item = GraphLocation file $ coerce $ (++ [item]) $ coerce bc

autolayout :: GraphLocation -> Empire ()
autolayout loc = do
    kids <- withGraph loc $ runASTOp $ do
        kids <- uses Graph.breadcrumbHierarchy (view BH.children)
        needLayout <- fmap catMaybes $ forM (Map.keys kids) $ \id -> do
            meta <- AST.getNodeMeta id
            return $ if meta /= def then Nothing else Just id
        autolayoutNodes needLayout
        return kids
    let next = concatMap (\(k, v) -> case v of
            BH.LambdaChild{}                -> [Breadcrumb.Lambda k]
            BH.ExprChild (BH.ExprItem pc _) -> map (Breadcrumb.Arg k) (Map.keys pc)) $ Map.assocs kids
    mapM_ (\a -> autolayout (loc |> a)) next

autolayoutTopLevel :: GraphLocation -> Empire ()
autolayoutTopLevel loc = do
    withUnit loc $ runASTOp $ do
        clsFuns    <- use Graph.clsFuns
        needLayout <- fmap catMaybes $ forM (Map.assocs clsFuns) $ \(id, (name, graph)) -> do
            f    <- ASTRead.getFunByName name
            meta <- AST.readMeta f
            let fileOffset = graph ^. Graph.fileOffset
            return $ if meta /= def then Nothing else Just (id, fileOffset)

        let sortedNeedLayout = sortOn snd needLayout
            positions  = map (Position.fromTuple . (,0)) [0,gapBetweenNodes..]
            autolayout = zipWith (\(id, _) pos -> (id,pos)) sortedNeedLayout positions
        mapM_ (uncurry setNodePositionCls) autolayout

printMarkedExpression :: GraphOp m => NodeRef -> m Text
printMarkedExpression ref = do
    exprMap <- getExprMap
    realRef <- IR.matchExpr ref $ \case
        IR.Marked _m expr -> IR.source expr
        _                 -> return ref
    expr    <- Text.pack <$> ASTPrint.printExpression realRef
    let markers = Map.keys $ Map.filter (== ref) exprMap
        marker  = case markers of
            (index:_) -> Text.pack $ "«" ++ show index ++ "»"
            _         -> ""
    return $ Text.concat [marker, expr]

data Sidebar = SidebarInput | SidebarOutput | NotSidebar
    deriving Eq

isSidebar :: GraphOp m => NodeId -> m Sidebar
isSidebar nodeId = do
    (input, output) <- GraphBuilder.getEdgePortMapping
    return $ if | input  == nodeId -> SidebarInput
                | output == nodeId -> SidebarOutput
                | otherwise        -> NotSidebar

isInput :: GraphOp m => NodeId -> m Bool
isInput nodeId = (== SidebarInput) <$> isSidebar nodeId

isOutput :: GraphOp m => NodeId -> m Bool
isOutput nodeId = (== SidebarOutput) <$> isSidebar nodeId

functionLocation :: GraphLocation -> GraphLocation
functionLocation (GraphLocation file (Breadcrumb b))
    | ((Breadcrumb.Definition f) : _) <- b = GraphLocation file (Breadcrumb [Breadcrumb.Definition f])
    | otherwise = GraphLocation file (Breadcrumb [])

previousOffset :: GraphOp m => NodeRef -> m Delta
previousOffset ref = do
    parents <- IR.getLayer @IR.Succs ref
    case toList parents of
        [] -> return mempty
        [parent] -> do
            inputs <- mapM IR.source =<< IR.inputs =<< IR.readTarget parent
            let lefts = takeWhile (/= ref) inputs
            spans  <- mapM readCodeSpan lefts
            let LeftSpacedSpan (SpacedSpan leftOff leftLen) = mconcat spans
            offset <- previousOffset =<< IR.readTarget parent
            LeftSpacedSpan (SpacedSpan o _) <- readCodeSpan =<< IR.readTarget parent
            return (leftOff + offset + o + leftLen)

readRangeProper :: GraphOp m => NodeRef -> m (LeftSpacedSpan Delta)
readRangeProper ref = do
    refSpan@(LeftSpacedSpan (SpacedSpan off len)) <- readCodeSpan ref
    moreOffset <- previousOffset ref
    let properOffset = off + moreOffset
        properSpan   = leftSpacedSpan properOffset len
    return properSpan

readRange :: GraphOp m => NodeRef -> m (Int, Int)
readRange ref = do
    LeftSpacedSpan (SpacedSpan offset len) <- readRangeProper ref
    fileOffset <- fromMaybe 0 <$> Code.getOffsetRelativeToFile ref
    return (fromIntegral (fileOffset), fromIntegral (fileOffset + len))

readCodeSpan :: GraphOp m => NodeRef -> m (LeftSpacedSpan Delta)
readCodeSpan ref = view CodeSpan.realSpan <$> IR.getLayer @CodeSpan ref

setCodeSpan :: GraphOp m => NodeRef -> LeftSpacedSpan Delta -> m ()
setCodeSpan ref s = IR.putLayer @CodeSpan ref $ CodeSpan.mkRealSpan s

getNodeIdForMarker :: GraphOp m => Int -> m (Maybe NodeId)
getNodeIdForMarker index = do
    exprMap      <- getExprMap
    let exprMap' :: Map.Map Luna.MarkerId NodeRef
        exprMap' = coerce exprMap
        Just ref = Map.lookup (fromIntegral index) exprMap'
    IR.matchExpr ref $ \case
        IR.Marked _m expr -> do
            expr'     <- IR.source expr
            nodeId    <- ASTRead.getNodeId expr'
            return nodeId

markerCodeSpan :: GraphLocation -> Int -> Empire (Int, Int)
markerCodeSpan loc index = withGraph loc $ runASTOp $ do
    exprMap      <- getExprMap
    let exprMap' :: Map.Map Luna.MarkerId NodeRef
        exprMap' = coerce exprMap
        Just ref = Map.lookup (fromIntegral index) exprMap'
    readRange ref

data MarkerNodeMeta = MarkerNodeMeta { marker :: Word64, meta :: NodeMeta }
    deriving (Eq, Show, Generic, FromJSON, ToJSON)
newtype FileMetadata = FileMetadata { metas :: [MarkerNodeMeta] }
    deriving (Show, Generic, FromJSON, ToJSON)

dumpMetadata :: FilePath -> Empire [MarkerNodeMeta]
dumpMetadata file = do
    funs <- withUnit (GraphLocation file (Breadcrumb [])) $ do
        funs <- use Graph.clsFuns
        return $ Map.keys funs
    metas <- forM funs $ \fun -> withGraph (GraphLocation file (Breadcrumb [Breadcrumb.Definition fun])) $ runASTOp $ do
        root       <- use $ Graph.breadcrumbHierarchy . BH.body
        oldMetas   <- extractMarkedMetasAndIds root
        return [ MarkerNodeMeta marker meta | (marker, (Just meta, _)) <- oldMetas ]
    return $ concat metas

addMetadataToCode :: FilePath -> Empire ()
addMetadataToCode file = do
    metadata <- FileMetadata <$> dumpMetadata file
    let metadataJSON           = (TL.toStrict . Aeson.encodeToLazyText . Aeson.toJSON) metadata
        metadataJSONWithHeader = Lexer.mkMetadata (Text.cons ' ' metadataJSON)
    withUnit (GraphLocation file (Breadcrumb [])) $ runASTOp $ do
        unit     <- use Graph.clsClass
        metaRef  <- ASTRead.getMetadataRef unit
        case metaRef of
            Just meta -> do
                metaStart <- Code.functionBlockStartRef meta
                LeftSpacedSpan (SpacedSpan off len) <- view CodeSpan.realSpan <$> IR.getLayer @CodeSpan meta
                Code.applyDiff metaStart (metaStart + len) metadataJSONWithHeader
                IR.putLayer @CodeSpan meta $ CodeSpan.mkRealSpan $ LeftSpacedSpan (SpacedSpan off (fromIntegral $ Text.length metadataJSONWithHeader))
            Nothing   -> do
                lastFun <- last <$> ASTRead.classFunctions unit
                lastFunStart <- Code.functionBlockStartRef lastFun
                LeftSpacedSpan (SpacedSpan off len) <- view CodeSpan.realSpan <$> IR.getLayer @CodeSpan lastFun
                let metaOffset = 2
                    metaStart  = lastFunStart + len
                    metadataJSONWithHeaderAndOffset = Text.concat [Text.replicate metaOffset "\n", metadataJSONWithHeader]
                Code.insertAt metaStart metadataJSONWithHeaderAndOffset
                meta <- IR.metadata metadataJSONWithHeader
                IR.putLayer @CodeSpan meta $ CodeSpan.mkRealSpan $ LeftSpacedSpan (SpacedSpan (fromIntegral metaOffset) (fromIntegral $ Text.length metadataJSONWithHeader))

                IR.matchExpr unit $ \case
                    IR.Unit _ _ cls -> do
                        cls' <- IR.source cls
                        Just (cls'' :: IR.Expr (IR.ClsASG)) <- IR.narrow cls'
                        l <- IR.unsafeGeneralize <$> IR.link meta cls''
                        links <- IR.matchExpr cls' $ \case
                            IR.ClsASG _ _ _ decls -> return decls
                        newFuns <- putNewFunctionRef l (Just lastFun) links
                        IR.modifyExprTerm cls'' $ wrapped . IR.termClsASG_decls .~ (map IR.unsafeGeneralize newFuns :: [IR.Link (IR.Expr IR.Draft) (IR.Expr Term.ClsASG)])

parseMetadata :: MonadIO m => Text -> m FileMetadata
parseMetadata meta =
    let json = Text.drop (Text.length "### META ") meta
        metadata = Aeson.eitherDecodeStrict' $ Text.encodeUtf8 json
    in case metadata of
        Right fm  -> return fm
        Left  err -> print err >> return (FileMetadata [])

readMetadata' :: ClassOp m => m FileMetadata
readMetadata' = do
    unit     <- use Graph.clsClass
    metaRef  <- ASTRead.getMetadataRef unit
    case metaRef of
        Just meta -> do
            metaStart <- Code.functionBlockStartRef meta
            LeftSpacedSpan (SpacedSpan off len) <- view CodeSpan.realSpan <$> IR.getLayer @CodeSpan meta
            code <- Code.getAt metaStart (metaStart + len)
            parseMetadata code
        _ -> return $ FileMetadata []

readMetadata :: FilePath -> Empire FileMetadata
readMetadata file = withUnit (GraphLocation file (Breadcrumb [])) $ runASTOp readMetadata'

unindent :: Int -> Text -> Text
unindent offset code = Text.unlines $ map (Text.drop offset) $ Text.lines code

data ImpossibleToInsert = ImpossibleToCollapse
    deriving (Show)

instance Exception ImpossibleToInsert where
    fromException = astExceptionFromException
    toException = astExceptionToException

findRefToInsertAfter :: GraphOp m => Set NodeRef -> Set NodeRef -> NodeRef -> m (Maybe NodeRef)
findRefToInsertAfter beforeNodes afterNodes ref = do
    if Set.null beforeNodes
      then return $ Just ref
      else IR.matchExpr ref $ \case
          IR.Seq l r -> do
              right <- IR.source r
              if Set.member right afterNodes
                then throwM ImpossibleToCollapse
                else findRefToInsertAfter (Set.delete right beforeNodes) afterNodes =<< IR.source l
          _ -> if Set.member ref afterNodes
                 then throwM ImpossibleToCollapse
                 else return Nothing

insertCodeBetween :: GraphOp m => [NodeId] -> [NodeId] -> Text -> m Text
insertCodeBetween beforeNodes afterNodes codeToInsert = do
    beforeRefs <- fmap Set.fromList $ forM beforeNodes ASTRead.getASTRef
    afterRefs  <- fmap Set.fromList $ forM afterNodes  ASTRead.getASTRef
    topSeq     <- use $ Graph.breadcrumbHierarchy . BH.body
    refToInsertAfter <- findRefToInsertAfter beforeRefs afterRefs topSeq
    insertPos        <- case refToInsertAfter of
        Nothing -> Code.getCurrentBlockBeginning
        Just r  -> do
            Just beg <- Code.getOffsetRelativeToFile r
            len      <- IR.getLayer @SpanLength r
            return $ beg + len
    Code.insertAt insertPos codeToInsert

generateCollapsedDefCode :: GraphOp m => [OutPortRef] -> [OutPortRef] -> [NodeId] -> m Text
generateCollapsedDefCode inputs outputs bodyIds = do
    inputNames <- fmap sort $ forM inputs $ \(OutPortRef (NodeLoc _ nodeId) pid) ->
        ASTRead.getASTOutForPort nodeId pid >>= ASTRead.getVarName
    outputNames <- forM outputs $ \(OutPortRef (NodeLoc _ nodeId) pid) ->
        ASTRead.getASTOutForPort nodeId pid >>= ASTRead.getVarName
    codeBegs <- fmap (sortOn fst) $ forM bodyIds $ \nid -> do
        ref     <- ASTRead.getASTRef nid
        Just cb <- Code.getOffsetRelativeToFile ref
        return (cb, ref)
    defName            <- generateNodeNameFromBase "func"
    currentIndentation <- Code.getCurrentIndentationLength
    let indentBy i l = "\n" <> Text.replicate (fromIntegral i) " " <> l
        topIndented  = indentBy currentIndentation
        bodyIndented = indentBy (currentIndentation + Code.defaultIndentationLength)
    newCodeBlockBody <- fmap Text.concat $ forM codeBegs $ \(beg, ref) -> do
        len  <- IR.getLayer @SpanLength ref
        code <- Code.getAt beg (beg + len)
        return $ bodyIndented code
    let header = topIndented $  "def "
                             <> Text.unwords (defName : fmap convert inputNames)
                             <> ":"
    returnBody <- case outputNames of
        []  -> do
            let lastNode = snd $ last codeBegs
            ASTRead.getNameOf lastNode
        [a] -> return $ Just $ convert a
        _   -> return $ Just $ "(" <> Text.intercalate ", " (convert <$> outputNames) <> ")"
    let returnLine = case returnBody of
            Just n -> bodyIndented n
            _      -> ""
    let defCode = header <> newCodeBlockBody <> returnLine
    let useLine = case outputNames of
            [] -> ""
            _  -> topIndented $ fromJust returnBody
                              <> " = "
                              <> Text.unwords (defName : fmap convert inputNames)
    return $ defCode <> useLine


collapseToFunction :: GraphLocation -> [NodeId] -> Empire ()
collapseToFunction loc nids = do
    when (null nids) $ throwM ImpossibleToCollapse
    code <- withGraph loc $ runASTOp $ do
        let ids = Set.fromList nids
        connections <- GraphBuilder.buildConnections
        let srcInIds = flip Set.member ids . view PortRef.srcNodeId . fst
            dstInIds = flip Set.member ids . view PortRef.dstNodeId . snd
            inConns  = filter (\x -> dstInIds x && not (srcInIds x)) connections
            inputs   = nub $ fst <$> inConns
            outConns = filter (\x -> srcInIds x && not (dstInIds x)) connections
            outputs  = nub $ fst <$> outConns
            useSites = outConns ^.. traverse . _2 . PortRef.dstNodeId
        newCode <- generateCollapsedDefCode inputs outputs nids
        insertCodeBetween useSites (view PortRef.srcNodeId <$> inputs) newCode
    reloadCode  loc code
    removeNodes loc nids


prepareCopy :: GraphLocation -> [NodeId] -> Empire String
prepareCopy loc@(GraphLocation _ (Breadcrumb [])) nodeIds = withUnit loc $ do
    clipboard <- runASTOp $ do
        starts  <- mapM Code.functionBlockStart nodeIds
        lengths <- do
            funs  <- use Graph.clsFuns
            let names       = map (\a -> (a, Map.lookup a funs)) nodeIds
                nonExistent = filter (isNothing . snd) names
            forM nonExistent $ \(nid, _) ->
                throwM $ BH.BreadcrumbDoesNotExistException (Breadcrumb [Breadcrumb.Definition nid])
            refs <- mapM ASTRead.getFunByName [ name | (_nid, Just (name, _graph)) <- names]
            forM refs $ \ref -> do
                LeftSpacedSpan (SpacedSpan off len) <- view CodeSpan.realSpan <$> IR.getLayer @CodeSpan ref
                return $ fromIntegral len
        codes <- mapM (\(start, len) -> Code.getAt start (start + len)) $ zip starts lengths
        return $ Text.intercalate "\n\n" codes
    return $ Text.unpack clipboard
prepareCopy loc nodeIds = withGraph loc $ do
    codesWithMeta <- runASTOp $ forM nodeIds $ \nid -> do
        ref    <- ASTRead.getASTRef nid
        code   <- Code.getCodeWithIndentOf ref
        indent <- Code.getCurrentIndentationLength
        let unindentedCode = unindent (fromIntegral indent) code
        metasWithMarkers   <- extractMarkedMetasAndIds ref
        return (unindentedCode, metasWithMarkers)
    let code  = Text.unlines $ map fst codesWithMeta
        metas = [ MarkerNodeMeta marker meta | (marker, (Just meta, _)) <- concat (map snd codesWithMeta) ]
        meta  = FileMetadata metas
        metadataJSON           = (TL.toStrict . Aeson.encodeToLazyText . Aeson.toJSON) meta
        metadataJSONWithHeader = Lexer.mkMetadata (Text.cons ' ' metadataJSON)
    return $ Text.unpack $ Text.concat [code, metadataJSONWithHeader]

moveToOrigin :: [MarkerNodeMeta] -> [MarkerNodeMeta]
moveToOrigin metas = map (\(MarkerNodeMeta m me) -> MarkerNodeMeta m (me & NodeMeta.position %~ Position.move (coerce (Position.rescale leftTopCorner (-1))))) metas
    where
        leftTopCorner = fromMaybe (Position.fromTuple (0,0))
                      $ Position.leftTopPoint
                      $ map (\mnm -> meta mnm ^. NodeMeta.position) metas

indent :: Int -> Text -> Text
indent offset (Text.lines -> header:rest) =
    Text.unlines $ header : map (\line -> Text.concat [Text.replicate offset " ", line]) rest
indent _      code = code

paste :: GraphLocation -> Position -> String -> Empire ()
paste loc@(GraphLocation file (Breadcrumb [])) position (Text.pack -> code) = do
    let funs = map (Text.stripEnd . Text.unlines)
             $ Split.split (Split.dropInitBlank $ Split.keepDelimsL $ Split.whenElt (\a -> maybe False (not . isSeparator . fst) $ Text.uncons a))
             $ Text.lines
             $ Code.removeMarkers code
        gaps = [0, gapBetweenNodes..]
    uuids <- forM (zip funs gaps) $ \(fun, gap) -> do
        uuid <- liftIO UUID.nextRandom
        let meta = set NodeMeta.position (Position.move (coerce $ Position.fromTuple (gap, 0)) position) def
        addFunNode loc ParseAsIs uuid fun meta
        return uuid
    forM uuids $ \uuid -> autolayout (GraphLocation file (Breadcrumb [Breadcrumb.Definition uuid]))
    resendCode loc
paste loc position (Text.pack -> code) = do
    withTC loc False $ do
        let lines = Text.splitOn "\n\n" code
            (metaLine, exprs) = partition (Text.isPrefixOf "### META") lines
        fm          <- forM (Safe.headMay metaLine) parseMetadata
        let metas   =  maybe [] (\(FileMetadata fm') -> moveToOrigin fm') fm
        indentation <- fromIntegral <$> runASTOp Code.getCurrentIndentationLength
        forM exprs $ \(Text.strip -> expr) -> do
            let (marker, rest) = Text.breakOn "»" expr & both %~ Text.drop 1
                mark           = Safe.readMay (Text.unpack marker) :: Maybe Word64
                newMeta        = case mark of
                    Just marker -> fromMaybe def $ fmap meta $ find (\(MarkerNodeMeta m me) -> m == marker) metas
                    _           -> def
                movedMeta      = newMeta & NodeMeta.position %~ Position.move (coerce position)
                nodeCode       = Code.removeMarkers $ indent indentation rest
            uuid <- liftIO UUID.nextRandom
            addNodeNoTC loc uuid nodeCode Nothing movedMeta
    autolayout loc
    resendCode loc

-- internal

getName :: GraphLocation -> NodeId -> Empire (Maybe Text)
getName loc nid = withGraph' loc (runASTOp $ GraphBuilder.getNodeName nid) $ use (Graph.clsFuns . ix nid . _1 . packed . re _Just)

generateNodeName :: GraphOp m => NodeRef -> m Text
generateNodeName = ASTPrint.genNodeBaseName >=> generateNodeNameFromBase

generateNodeNameFromBase :: GraphOp m => Text -> m Text
generateNodeNameFromBase base = do
    ids   <- uses Graph.breadcrumbHierarchy BH.topLevelIDs
    names <- Set.fromList . catMaybes <$> mapM GraphBuilder.getNodeName ids
    let allPossibleNames = zipWith (<>) (repeat base) (convert . show <$> [1..])
        Just newName     = find (not . flip Set.member names) allPossibleNames
    return newName

runTC :: GraphLocation -> Bool -> Command ClsGraph ()
runTC loc flush = do
    g <- get
    Publisher.requestTC loc g flush

printNodeLine :: GraphOp m => NodeId -> m String
printNodeLine nodeId = GraphUtils.getASTPointer nodeId >>= ASTPrint.printExpression

withTC' :: GraphLocation -> Bool -> Command Graph a -> Command ClsGraph a -> Empire a
withTC' loc@(GraphLocation file bs) flush actG actC = do
    res <- withGraph' loc actG actC
    withGraph' (GraphLocation file def) (return ()) (runTC loc flush)
    return res

withTCUnit :: GraphLocation -> Bool -> Command ClsGraph a -> Empire a
withTCUnit loc flush cmd = withTC' loc flush (throwM UnsupportedOperation) cmd

withTC :: GraphLocation -> Bool -> Command Graph a -> Empire a
withTC loc flush actG = withTC' loc flush actG (throwM UnsupportedOperation)

data UnsupportedOperation = UnsupportedOperation
    deriving Show

instance Exception UnsupportedOperation where
    fromException = astExceptionFromException
    toException = astExceptionToException

withGraph :: GraphLocation -> Command Graph a -> Empire a
withGraph (GraphLocation file breadcrumb) act = withBreadcrumb file breadcrumb act (throwM UnsupportedOperation)

withUnit :: GraphLocation -> Command ClsGraph a -> Empire a
withUnit (GraphLocation file breadcrumb) act = withBreadcrumb file breadcrumb (throwM UnsupportedOperation) act

withGraph' :: GraphLocation -> Command Graph a -> Command ClsGraph a -> Empire a
withGraph' (GraphLocation file breadcrumb) actG actC = withBreadcrumb file breadcrumb actG actC

getOutEdges :: GraphOp m => NodeId -> m [InPortRef]
getOutEdges nodeId = do
    edges <- GraphBuilder.buildConnections
    let filtered = filter (\(opr, _) -> opr ^. PortRef.srcNodeId == nodeId) edges
    return $ view _2 <$> filtered

disconnectPort :: GraphOp m => InPortRef -> m ()
disconnectPort (InPortRef (NodeLoc _ dstNodeId) dstPort) = case dstPort of
    []        -> setToNothing dstNodeId
    _         -> removeInternalConnection dstNodeId dstPort

setToNothing :: GraphOp m => NodeId -> m ()
setToNothing dst = do
    (_, out) <- GraphBuilder.getEdgePortMapping
    let disconnectOutputEdge = out == dst
        nothingExpr          = "None"
    nothing <- IR.generalize <$> IR.cons_ (convert nothingExpr)
    IR.putLayer @SpanLength nothing $ convert $ Text.length nothingExpr
    if disconnectOutputEdge
        then setOutputTo nothing
        else GraphUtils.rewireNode dst nothing

removeInternalConnection :: GraphOp m => NodeId -> InPortId -> m ()
removeInternalConnection nodeId port = do
    dstAst <- ASTRead.getTargetEdge nodeId
    beg    <- Code.getASTTargetBeginning nodeId
    ASTBuilder.removeArgument dstAst beg port

makeInternalConnection :: GraphOp m => NodeId -> NodeId -> OutPortId -> InPortId -> m ()
makeInternalConnection src dst outPort inPort = do
    dstBeg <- Code.getASTTargetBeginning dst
    srcAst <- ASTRead.getASTOutForPort src outPort
    dstAst <- ASTRead.getTargetEdge dst
    ASTBuilder.makeConnection dstAst dstBeg inPort srcAst

makeWhole :: GraphOp m => NodeId -> NodeId -> OutPortId -> m ()
makeWhole src dst outPort = do
    (_, out) <- GraphBuilder.getEdgePortMapping
    let connectToOutputEdge = out == dst
    srcAst   <- ASTRead.getASTOutForPort src outPort
    if connectToOutputEdge then setOutputTo srcAst else GraphUtils.rewireNode dst srcAst

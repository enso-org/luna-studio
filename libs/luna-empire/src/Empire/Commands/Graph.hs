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
    ( addImports
    , addNode
    , addNodeCondTC
    , addNodeWithConnection
    , addPort
    , addPortWithConnections
    , addSubgraph
    , autolayout
    , autolayoutNodes
    , autolayoutTopLevel
    , removeNodes
    , movePort
    , removePort
    , renamePort
    , getPortName
    , setNodeExpression
    , setNodeMeta
    , setNodePosition
    , connect
    , connectPersistent
    , connectCondTC
    , connectNoTC
    , decodeLocation
    , disconnect
    , getAvailableImports
    , getNodeMeta
    , getNodeMetas
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
    , typecheckWithRecompute
    , getName
    , MarkerNodeMeta(..)
    , FileMetadata(..)
    , dumpMetadata
    , addMetadataToCode
    , readMetadata
    , prepareCopy
    , paste
    , copyText
    , pasteText
    , collapseToFunction
    , moveToOrigin
    , getImports
    , getAvailableImports
    , setInterpreterState
    , stripMetadata
    , prepareGraphError
    , prepareLunaError
    , reloadCode
    , resendCode
    , prepareNodeCache
    , prepareLunaError
    , importsToHints
    , filterPrimMethods
    ) where

import           Control.Arrow                    ((&&&), (***))
import           Control.Concurrent               (readMVar)
import qualified Control.Concurrent.MVar.Lifted   as Lifted
import           Control.Monad                    (forM)
import           Control.Monad.Catch              (handle, try)
import           Control.Monad.State              hiding (when)
import           Data.Aeson                       (FromJSON, ToJSON)
import qualified Data.Aeson                       as Aeson
import qualified Data.Aeson.Text                  as Aeson
import qualified Data.Bimap                       as Bimap
import           Data.Coerce                      (coerce)
import           Data.Char                        (isSeparator, isSpace, isUpper)
import           Data.Foldable                    (toList)
import           Data.List                        ((++), elemIndex, find, group, partition, sortBy, sortOn, nub, head)
import qualified Data.List                        as List
import qualified Data.List.Split                  as Split
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
import           Empire.ASTOp                     (ASTOp, ClassOp, GraphOp, putNewIR, putNewIRCls, runASTOp, runAliasAnalysis, runModuleTypecheck)
import qualified Empire.ASTOps.Builder            as ASTBuilder
import           Empire.ASTOps.BreadcrumbHierarchy (getMarker, prepareChild, isNone)
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
                                                   NotUnifyException, PortDoesNotExistException(..), ConnectionException(..),
                                                   SomeASTException, astExceptionFromException, astExceptionToException)
import qualified Empire.Data.BreadcrumbHierarchy  as BH
import           Empire.Data.Graph                (ClsGraph, Graph)
import qualified Empire.Data.Graph                as Graph
import           Empire.Data.Layers               (SpanLength, SpanOffset)
import qualified Empire.Data.Library              as Library
import           Empire.Empire
import           Empire.Prelude                   hiding (head, toList)
import qualified Luna.Builtin.Data.Class          as IR
import qualified Luna.Builtin.Data.Function       as IR
import qualified Luna.Builtin.Data.Module         as Module
import qualified Luna.Compilation                 as Compilation
import qualified Luna.IR                          as IR
import qualified Luna.IR.Term.Core                as Term
import qualified Luna.IR.Term.Unit                as Term
import qualified Luna.Project                     as Project
import           Luna.Syntax.Text.Analysis.SpanTree (Spanned(..))
import qualified Luna.Syntax.Text.Analysis.SpanTree as SpanTree
import qualified Luna.Syntax.Text.Lexer           as Lexer
import           Luna.Syntax.Text.Parser.CodeSpan (CodeSpan)
import qualified Luna.Syntax.Text.Parser.CodeSpan as CodeSpan
import           Luna.Syntax.Text.Parser.Marker   (MarkedExprMap (..))
import qualified Luna.Syntax.Text.Parser.Marker   as Luna
import qualified LunaStudio.API.Control.Interpreter as Interpreter
import           LunaStudio.Data.Breadcrumb       (Breadcrumb (..), BreadcrumbItem, Named)
import qualified LunaStudio.Data.Breadcrumb       as Breadcrumb
import           LunaStudio.Data.Constants        (gapBetweenNodes)
import           LunaStudio.Data.Connection       (Connection (..))
import           LunaStudio.Data.Diff             (Diff (..))
import qualified LunaStudio.Data.Error            as ErrorAPI
import qualified LunaStudio.Data.Graph            as APIGraph
import           LunaStudio.Data.GraphLocation    (GraphLocation (..))
import           LunaStudio.Data.Node             (ExpressionNode (..), NodeId)
import qualified LunaStudio.Data.Node             as Node
import           LunaStudio.Data.NodeCache        (NodeCache (..), nodeMetaMap, nodeIdMap)
import           LunaStudio.Data.NodeLoc          (NodeLoc (..))
import qualified LunaStudio.Data.NodeLoc          as NodeLoc
import           LunaStudio.Data.NodeMeta         (NodeMeta)
import qualified LunaStudio.Data.NodeMeta         as NodeMeta
import           LunaStudio.Data.NodeSearcher     (ImportName, ImportsHints, ClassHints(..), ModuleHints(..))
import           LunaStudio.Data.Point            (Point)
import           LunaStudio.Data.Port             (InPortId, InPortIndex (..), getPortNumber)
import qualified LunaStudio.Data.Port             as Port
import           LunaStudio.Data.PortDefault      (PortDefault)
import           LunaStudio.Data.PortRef          (AnyPortRef (..), InPortRef (..), OutPortRef (..))
import qualified LunaStudio.Data.PortRef          as PortRef
import           LunaStudio.Data.Position         (Position)
import qualified LunaStudio.Data.Position         as Position
import           LunaStudio.Data.Range            (Range(..))
import qualified LunaStudio.Data.Range            as Range
import qualified OCI.IR.Combinators               as IR (replaceSource, deleteSubtree, narrow, replace)
import           OCI.IR.Name.Qualified            (QualName)
import qualified Path
import qualified Safe
import           System.Directory                 (canonicalizePath)
import           System.Environment               (getEnv)
import           GHC.Stack                        (renderStack, whoCreated)

addImports :: GraphLocation -> [Text] -> Empire ()
addImports loc@(GraphLocation file _) modulesToImport = do
    newCode <- withUnit (GraphLocation file def) $ do
        existingImports <- runASTOp getImportsInFile
        let imports = nativeModuleName : "Std.Base" : existingImports
        let neededImports = filter (`notElem` imports) modulesToImport
        code <- use Graph.code
        let newImports = map (\i -> Text.concat ["import ", i, "\n"]) neededImports
        return $ Text.concat $ newImports ++ [code]
    reloadCode loc newCode
    typecheckWithRecompute loc
    withUnit (GraphLocation file def) $ do
        modulesMVar <- view modules
        importPaths <- liftIO $ getImportPaths loc
        Lifted.modifyMVar modulesMVar $ \cmpModules -> do
            res     <- runModuleTypecheck importPaths cmpModules
            case res of
                Left err                          -> liftIO (print err) >> return (cmpModules, ())
                Right (newImports, newCmpModules) -> return (newCmpModules, ())


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
                _        -> Code.functionBlockStartRef =<< ASTRead.classFromUnit unit
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
            let newOffset = if funBlockStart == 0 then 0 else off
            IR.putLayer @CodeSpan function $ CodeSpan.mkRealSpan (LeftSpacedSpan (SpacedSpan newOffset funLen))
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
    (name, markedFunction, markedCode) <- runASTOp $ do
        name <- ASTRead.cutThroughDocAndMarked parse >>= \x -> IR.matchExpr x $ \case
            IR.ASGRootedFunction n _ -> do
                name <- ASTRead.getVarName' =<< IR.source n
                return $ nameToString name
        index  <- getNextTopLevelMarker
        marker <- IR.marker' index
        let markerText = Code.makeMarker index
            markerLen  = convert $ Text.length markerText
        IR.putLayer @CodeSpan marker $ CodeSpan.mkRealSpan (LeftSpacedSpan (SpacedSpan 0 markerLen))
        markedNode <- IR.marked' marker parse
        when (meta /= def) $ AST.writeMeta markedNode meta
        Graph.clsCodeMarkers . at index ?= markedNode
        IR.putLayer @CodeSpan markedNode $ CodeSpan.mkRealSpan (LeftSpacedSpan (SpacedSpan 0 (markerLen + convert (Text.length code))))
        let markedCode = Text.concat [markerText, code]
        return (name, markedNode, markedCode)

    unit <- use Graph.clsClass
    (insertedCharacters, codePosition) <- runASTOp $ do
        funs <- ASTRead.classFunctions unit
        previousFunction <- findPreviousFunction meta funs

        insertedCharacters <- insertFunAfter previousFunction markedFunction markedCode
        cls' <- ASTRead.classFromUnit unit
        Just (cls'' :: IR.Expr (IR.ClsASG)) <- IR.narrow cls'
        l <- IR.unsafeGeneralize <$> IR.link markedFunction cls''
        links <- IR.matchExpr cls' $ \case
            IR.ClsASG _ _ _ _ decls -> return decls
        newFuns <- putNewFunctionRef l previousFunction links
        IR.modifyExprTerm cls'' $ wrapped . IR.termClsASG_decls .~ (map IR.unsafeGeneralize newFuns :: [IR.Link (IR.Expr IR.Draft) (IR.Expr Term.ClsASG)])
        codePosition       <- Code.functionBlockStartRef markedFunction

        return (fromIntegral insertedCharacters, codePosition)

    Graph.clsFuns . traverse . Graph.funGraph . Graph.fileOffset %= (\off -> if off >= codePosition then off + insertedCharacters else off)
    (uuid', graph) <- makeGraphCls markedFunction (Just uuid)

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

addNodeWithConnection :: GraphLocation -> NodeLoc -> Text -> NodeMeta -> Maybe NodeId -> Empire ExpressionNode
addNodeWithConnection location nl@(NodeLoc _ nodeId) expression nodeMeta connectTo = do
    node <- addNodeCondTC False location nodeId expression nodeMeta
    for_ connectTo $ \nid -> do
        handle (\(e :: SomeASTException) -> return ()) $ do
            pattern <- withGraph location $ runASTOp $ ASTRead.nodeIsPatternMatch nid
            when pattern $ throwM InvalidConnectionException
            let firstWord = unsafeHead $ Text.words expression
            let shouldConnectToArg w = isUpper (Text.head w)
                ports = node ^.. Node.inPorts . traverse . Port.portId
                selfs = filter (\a -> all (== Self) a && not (null a)) ports
                longestSelfChain = Safe.headDef [Self] $ reverse $ sortBy (compare `on` length) selfs
                port = if shouldConnectToArg firstWord then [Arg 0] else longestSelfChain
            void $ connectCondTC False location (OutPortRef (NodeLoc def nid) []) (InPortRef' $ InPortRef nl port)
            withGraph location $ runASTOp $ autolayoutNodesAST [nodeId]
    typecheck location
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
insertAfter s after new textBeginning code = do
    indentBy <- Code.getCurrentIndentationLength
    IR.matchExpr s $ \case
        IR.Seq l r -> do
            rt <- IR.source r
            if Just rt == after
                then do
                    Code.insertAt (fromIntegral textBeginning) ("\n" <> Text.replicate (fromIntegral indentBy) " " <> code)
                    newSeq <- IR.generalize <$> IR.seq s new
                    IR.putLayer @SpanLength newSeq =<< IR.getLayer @SpanLength s
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
            if after == Just s
                then do
                    Code.insertAt (fromIntegral textBeginning) ("\n" <> Text.replicate (fromIntegral indentBy) " " <> code)
                    newSeq <- IR.generalize <$> IR.seq s new
                    IR.putLayer @SpanLength newSeq =<< IR.getLayer @SpanLength s
                    setSeqOffsets newSeq 0 (indentBy + 1)
                    return (newSeq, True)
                else do
                    slen <- IR.getLayer @SpanLength s
                    Code.insertAt (fromIntegral $ textBeginning - slen) (code <> "\n" <> Text.replicate (fromIntegral indentBy) " ")
                    newSeq <- IR.generalize <$> IR.seq new s
                    IR.putLayer @SpanLength newSeq =<< IR.getLayer @SpanLength s
                    setSeqOffsets newSeq 0 (indentBy + 1)
                    return (newSeq, True)

insertAfterAndUpdate :: GraphOp m => NodeRef -> Maybe NodeRef -> NodeRef -> Delta -> Text -> m ()
insertAfterAndUpdate s after new textBeginning code = do
    (newS, shouldUpdate) <- insertAfter s after new textBeginning code
    when shouldUpdate $ updateGraphSeq $ Just newS
    indentBy <- Code.getCurrentIndentationLength
    newLen <- IR.getLayer @SpanLength new
    Code.gossipLengthsChangedBy (newLen + indentBy + 1) newS

getCurrentFunctionOutput :: GraphOp m => m NodeRef
getCurrentFunctionOutput = do
    seq <- ASTRead.getCurrentBody
    IR.matchExpr seq $ \case
        IR.Seq _ r -> IR.source r
        _ -> return seq

putInSequence :: GraphOp m => NodeRef -> Text -> NodeMeta -> m ()
putInSequence ref code meta = do
    oldSeq             <- ASTRead.getCurrentBody
    nodes              <- AST.readSeq oldSeq
    nodesAndMetas      <- mapM (\n -> (n,) <$> AST.readMeta n) nodes
    let nodesWithMetas =  mapMaybe (\(n,m) -> (n,) <$> m) nodesAndMetas
    nearestNode        <- findPreviousNodeInSequence oldSeq meta nodesWithMetas
    blockEnd           <- Code.getCurrentBlockEnd
    currentOutput      <- getCurrentFunctionOutput
    anonOutput         <- ASTRead.isAnonymous currentOutput
    none               <- isNone currentOutput
    insertAfterAndUpdate oldSeq nearestNode ref blockEnd code
    when (Just currentOutput == nearestNode && anonOutput && not none) $ do
        Just nid <- GraphBuilder.getNodeIdWhenMarked currentOutput
        ASTBuilder.ensureNodeHasName generateNodeName nid
        v <- ASTRead.getASTVar nid
        setOutputTo v

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
    oldSeq   <- ASTRead.getCurrentBody
    blockEnd <- Code.getCurrentBlockEnd
    newSeq   <- reconnectOut oldSeq out blockEnd
    traverse_ (updateGraphSeq . Just) newSeq

updateGraphSeqWithWhilelist :: GraphOp m => [NodeRef] -> Maybe NodeRef -> m ()
updateGraphSeqWithWhilelist whitelist newOut = do
    currentTgt   <- ASTRead.getCurrentASTTarget
    Just outLink <- ASTRead.getFirstNonLambdaLink currentTgt
    oldSeq       <- IR.source outLink
    case newOut of
        Just o  -> do
            oldSeqLen <- IR.getLayer @SpanLength oldSeq
            newSeqLen <- IR.getLayer @SpanLength o
            Code.gossipLengthsChangedBy (newSeqLen - oldSeqLen) =<< IR.readTarget outLink
            IR.replaceSource o outLink
        Nothing -> do
            none <- IR.generalize <$> IR.cons_ "None"
            let noneLen = fromIntegral $ length ("None"::String)
            IR.replaceSource none outLink
            Code.gossipLengthsChangedBy noneLen none
            blockEnd <- Code.getCurrentBlockEnd
            Code.insertAt (blockEnd - noneLen) "None"
            return ()
    IR.deepDeleteWithWhitelist oldSeq $ Set.union (Set.fromList whitelist) (Set.fromList (maybeToList newOut))
    oldRef <- use $ Graph.breadcrumbHierarchy . BH.self
    when (oldRef == oldSeq) $ for_ newOut (Graph.breadcrumbHierarchy . BH.self .=)

updateGraphSeq :: GraphOp m => Maybe NodeRef -> m ()
updateGraphSeq newOut = updateGraphSeqWithWhilelist [] newOut

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

addPort :: GraphLocation -> OutPortRef -> Empire ()
addPort loc portRef = addPortWithConnections loc portRef Nothing []

addPortNoTC :: GraphLocation -> OutPortRef -> Maybe Text -> Command Graph ()
addPortNoTC loc (OutPortRef nl pid) name = runASTOp $ do
    let nid      = nl ^. NodeLoc.nodeId
        position = getPortNumber pid
    (inE, _) <- GraphBuilder.getEdgePortMapping
    when (inE /= nid) $ throwM NotInputEdgeException
    ref <- ASTRead.getCurrentASTTarget
    ASTBuilder.detachNodeMarkersForArgs ref
    ids      <- uses Graph.breadcrumbHierarchy BH.topLevelIDs
    varNames <- catMaybes <$> mapM GraphBuilder.getNodeName ids
    ASTModify.addLambdaArg position ref name $ map convert varNames
    newLam <- ASTRead.getCurrentASTTarget
    ASTBuilder.attachNodeMarkersForArgs nid [] newLam

addPortWithConnections :: GraphLocation -> OutPortRef -> Maybe Text -> [AnyPortRef] -> Empire ()
addPortWithConnections loc portRef name connectTo = do
    withTC loc False $ do
        addPortNoTC loc portRef name
        for_ connectTo $ connectNoTC loc portRef
    resendCode loc

addSubgraph :: GraphLocation -> [ExpressionNode] -> [Connection] -> Empire [ExpressionNode]
addSubgraph loc@(GraphLocation _ (Breadcrumb [])) nodes _ = do
    res <- forM nodes $ \n -> addFunNode loc ParseAsIs (n ^. Node.nodeId) (n ^. Node.code) (n ^. Node.nodeMeta)
    resendCode loc
    return res
addSubgraph loc nodes conns = do
    newNodes <- withTC loc False $ do
        newNodes <- forM nodes $ \n -> addNodeNoTC loc (n ^. Node.nodeId) (n ^. Node.code) (n ^. Node.name) (n ^. Node.nodeMeta)
        for_ conns $ \(Connection src dst) -> connectNoTC loc src (InPortRef' dst)
        return newNodes
    resendCode loc
    return newNodes

removeNodes :: GraphLocation -> [NodeId] -> Empire ()
removeNodes loc@(GraphLocation file (Breadcrumb [])) nodeIds = do
    withUnit loc $ do
        funs <- use Graph.clsFuns

        let graphsToRemove = Map.elems $ Map.filterWithKey (\a _ -> a `elem` nodeIds) funs
        Graph.clsFuns .= Map.filterWithKey (\a _ -> a `notElem` nodeIds) funs

        unit <- use Graph.clsClass
        runASTOp $ do
            cls' <- ASTRead.classFromUnit unit
            Just (cls'' :: IR.Expr (IR.ClsASG)) <- IR.narrow cls'
            funs <- IR.matchExpr cls' $ \case
                IR.ClsASG _ _ _ _ f -> do
                    links <- mapM (\link -> (link,) <$> IR.source link) f
                    forM links $ \(link, fun) -> do
                        nid <- ASTRead.getNodeId fun
                        return $ case nid of
                            Just i -> if i `elem` nodeIds then Left link else Right link
                            _      -> Right link
            let (toRemove, left) = partitionEithers funs
            spans <- forM toRemove $ \candidate -> do
                ref <- IR.source candidate
                start <- Code.functionBlockStartRef ref
                LeftSpacedSpan (SpacedSpan off len) <- view CodeSpan.realSpan <$> IR.getLayer @CodeSpan.CodeSpan ref
                return (start - off, start + len)
            forM (reverse spans) $ \(start, end) -> do
                let removedCharacters = end - start
                Graph.clsFuns . traverse . Graph.funGraph . Graph.fileOffset %= (\off -> if off > end then off - removedCharacters else off)
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
    traverse_ disconnectPort obsoleteEdges
    mapM deepRemoveExprMarkers =<< use (Graph.breadcrumbHierarchy . BH.children . at nodeId)
    Graph.breadcrumbHierarchy . BH.children . at nodeId .= Nothing
    removeFromSequence astRef
    return $ map (view PortRef.dstNodeId) obsoleteEdges

removeExprMarker :: GraphOp m => NodeRef -> m ()
removeExprMarker ref = do
    exprMap <- getExprMap
    let newExprMap = Map.filter (/= ref) exprMap
    setExprMap newExprMap

unpinSequenceElement :: GraphOp m => NodeRef -> NodeRef -> m (Maybe NodeRef, Bool)
unpinSequenceElement seq ref = IR.matchExpr seq $ \case
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
                recur <- unpinSequenceElement lt ref
                case recur of
                    (Just newRef, True) -> do -- left child changed
                        len    <- IR.getLayer @SpanLength ref
                        indent <- Code.getCurrentIndentationLength
                        Code.gossipLengthsChangedBy (negate $ len + indent + 1) lt
                        ASTModify.substitute newRef lt
                        IR.delete lt
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

unpinFromSequence :: GraphOp m => NodeRef -> m ()
unpinFromSequence ref = do
    oldSeq <- ASTRead.getCurrentBody
    (newS, shouldUpdate) <- unpinSequenceElement oldSeq ref
    when shouldUpdate (updateGraphSeqWithWhilelist [ref] newS)

removeFromSequence :: GraphOp m => NodeRef -> m ()
removeFromSequence ref = do
    unpinFromSequence ref
    IR.deleteSubtree ref

removePort :: GraphLocation -> OutPortRef -> Empire ()
removePort loc portRef = do
    withTC loc False $ runASTOp $ do
        let nodeId = portRef ^. PortRef.srcNodeId
        ref <- ASTRead.getCurrentASTTarget
        ASTBuilder.detachNodeMarkersForArgs ref
        (inE, _) <- GraphBuilder.getEdgePortMapping
        if nodeId == inE then ASTModify.removeLambdaArg (portRef ^. PortRef.srcPortId) ref
                         else throwM NotInputEdgeException
        newLam <- ASTRead.getCurrentASTTarget
        ASTBuilder.attachNodeMarkersForArgs nodeId [] newLam
        GraphBuilder.buildInputSidebar nodeId
    resendCode loc

movePort :: GraphLocation -> OutPortRef -> Int -> Empire ()
movePort loc portRef newPosition = do
    withTC loc False $ runASTOp $ do
        let nodeId = portRef ^. PortRef.srcNodeId
        ref        <- ASTRead.getCurrentASTTarget
        (input, _) <- GraphBuilder.getEdgePortMapping
        if nodeId == input then ASTModify.moveLambdaArg (portRef ^. PortRef.srcPortId) newPosition ref
                           else throwM NotInputEdgeException
        ref        <- ASTRead.getCurrentASTTarget
        ASTBuilder.attachNodeMarkersForArgs nodeId [] ref
        GraphBuilder.buildInputSidebar nodeId
    resendCode loc

renamePort :: GraphLocation -> OutPortRef -> Text -> Empire ()
renamePort loc portRef newName = do
    withTC loc False $ runASTOp $ do
        let nodeId = portRef ^. PortRef.srcNodeId
        ref        <- ASTRead.getCurrentASTTarget
        (input, _) <- GraphBuilder.getEdgePortMapping
        if nodeId == input then ASTModify.renameLambdaArg (portRef ^. PortRef.srcPortId) (Text.unpack newName) ref
                           else throwM NotInputEdgeException
        GraphBuilder.buildInputSidebar nodeId
    resendCode loc

getPortName :: GraphLocation -> OutPortRef -> Empire Text
getPortName loc portRef = do
    withGraph loc $ runASTOp $ do
        let nodeId = portRef ^. PortRef.srcNodeId
            portId = portRef ^. PortRef.srcPortId
            arg    = getPortNumber portId
        ref        <- ASTRead.getCurrentASTTarget
        (input, _) <- GraphBuilder.getEdgePortMapping
        portsNames <- GraphBuilder.getPortsNames ref
        if nodeId == input
            then maybe (throwM $ PortDoesNotExistException portId) (return . convert) $ Safe.atMay portsNames arg
            else throwM NotInputEdgeException

setNodeExpression :: GraphLocation -> NodeId -> Text -> Empire ExpressionNode
setNodeExpression loc@(GraphLocation file _) nodeId expr' = do
    let expression = Text.strip expr'
    newCode <- withGraph loc $ runASTOp $ do
        oldExpr   <- ASTRead.getASTTarget nodeId
        oldBegin  <- Code.getASTTargetBeginning nodeId
        oldLen    <- IR.getLayer @SpanLength oldExpr
        let oldEnd = oldBegin + oldLen
        Code.applyDiff oldBegin oldEnd expression
    reloadCode loc newCode
    resendCode loc
    withGraph loc $ runASTOp $ GraphBuilder.buildNode nodeId

updateExprMap :: GraphOp m => NodeRef -> NodeRef -> m ()
updateExprMap new old = do
    exprMap <- getExprMap
    let updated = Map.map (\a -> if a == old then new else a) exprMap
    setExprMap updated

resendCode :: GraphLocation -> Empire ()
resendCode loc = resendCodeWithCursor loc Nothing

resendCodeWithCursor :: GraphLocation -> Maybe Point -> Empire ()
resendCodeWithCursor loc@(GraphLocation file _) cursor = do
    code <- getCode loc
    Publisher.notifyCodeUpdate file
                               code
                               cursor

setNodeMetaGraph :: GraphOp m => NodeId -> NodeMeta -> m ()
setNodeMetaGraph nodeId newMeta = do
    ref <- ASTRead.getASTRef nodeId
    AST.writeMeta ref newMeta

setNodeMetaFun :: NodeId -> NodeMeta -> Command ClsGraph ()
setNodeMetaFun nodeId newMeta = runASTOp $ do
    Just fun <- use $ Graph.clsFuns . at nodeId
    f        <- ASTRead.getFunByNodeId nodeId
    AST.writeMeta f newMeta

setNodeMeta :: GraphLocation -> NodeId -> NodeMeta -> Empire ()
setNodeMeta loc nodeId newMeta = withGraph' loc (runASTOp $ setNodeMetaGraph nodeId newMeta) (setNodeMetaFun nodeId newMeta)

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
    Just fun <- use $ Graph.clsFuns . at nodeId
    f        <- ASTRead.getFunByNodeId nodeId
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

reorder :: GraphOp m => NodeId -> NodeId -> m ()
reorder dst src = do
    dstDownstream <- getNodeDownstream dst
    let wouldIntroduceCycle = src `elem` dstDownstream
    if wouldIntroduceCycle then return () else do
        dstDownAST    <- mapM ASTRead.getASTRef dstDownstream
        nodes         <- ASTRead.getCurrentBody >>= AST.readSeq
        srcAst        <- ASTRead.getASTRef src
        let srcIndex    = List.elemIndex srcAst nodes
            downIndices = sortOn snd $ map (\a -> (a, a `List.elemIndex` nodes)) dstDownAST
            leftToSrc   = map fst $ filter ((< srcIndex) . snd) downIndices
        let f acc e = do
                oldSeq   <- ASTRead.getCurrentBody
                code     <- Code.getCodeOf e
                unpinFromSequence e
                blockEnd <- Code.getCurrentBlockEnd
                oldSeq'   <- ASTRead.getCurrentBody
                insertAfterAndUpdate oldSeq' (Just acc) e blockEnd code
                return e
        foldM f srcAst leftToSrc
        return ()

getNodeDownstream :: GraphOp m => NodeId -> m [NodeId]
getNodeDownstream nodeId = do
    conns <- map (\(a,b) -> (a^.PortRef.srcNodeId, b^.PortRef.dstNodeId)) <$> GraphBuilder.buildConnections
    (_, output) <- GraphBuilder.getEdgePortMapping
    let connsWithoutOutput = filter ((/= output) . snd) conns
        go c n             = let next = map snd $ filter ((== n) . fst) c
                             in next ++ concatMap (go c) next
    return $ nodeId : go connsWithoutOutput nodeId

connectPersistent :: GraphOp m => OutPortRef -> AnyPortRef -> m Connection
connectPersistent src@(OutPortRef (NodeLoc _ srcNodeId) srcPort) (InPortRef' dst@(InPortRef (NodeLoc _ dstNodeId) dstPort)) = do
    -- FIXME[MK]: passing the `generateNodeName` here is a hack arising from cyclic module deps. Need to remove together with modules refactoring.
    whenM (not <$> ASTRead.isInputSidebar srcNodeId) $ do
        ref   <- ASTRead.getASTRef srcNodeId
        s     <- ASTRead.getCurrentBody
        nodes <- AST.readSeq s
        ASTBuilder.ensureNodeHasName generateNodeName srcNodeId
        when (unsafeLast nodes == ref) $ do
            var <- ASTRead.getASTVar srcNodeId
            setOutputTo var
    srcAst <- ASTRead.getASTOutForPort srcNodeId srcPort
    (input, output) <- GraphBuilder.getEdgePortMapping
    when (input /= srcNodeId && output /= dstNodeId) $ do
        src'   <- ASTRead.getASTRef srcNodeId
        dstAst <- ASTRead.getASTRef dstNodeId
        s      <- ASTRead.getCurrentBody
        nodes  <- AST.readSeq s
        let srcPos = List.elemIndex src' nodes
            dstPos = List.elemIndex dstAst nodes
        when (dstPos < srcPos) $ reorder dstNodeId srcNodeId
    case dstPort of
        [] -> makeWhole srcAst dstNodeId
        _  -> makeInternalConnection srcAst dstNodeId dstPort
    return $ Connection src dst
connectPersistent src@(OutPortRef (NodeLoc _ srcNodeId) srcPort) (OutPortRef' dst@(OutPortRef d@(NodeLoc _ dstNodeId) dstPort)) = do
    case dstPort of
        []    -> do
            ASTBuilder.flipNode dstNodeId
            connectPersistent src (InPortRef' (InPortRef d []))
        _ : _ -> throwM InvalidConnectionException


connectNoTC :: GraphLocation -> OutPortRef -> AnyPortRef -> Command Graph Connection
connectNoTC loc outPort anyPort = runASTOp $ do
    (inputSidebar, outputSidebar) <- GraphBuilder.getEdgePortMapping
    let OutPortRef (convert -> outNodeId) outPortId = outPort
        inNodeId = anyPort ^. PortRef.nodeId
        inPortId = anyPort ^. PortRef.portId
    let codeForId id | id == inputSidebar  = return "input sidebar"
                     | id == outputSidebar = return "output sidebar"
                     | otherwise           = ASTRead.getASTPointer id >>= Code.getCodeOf
    outNodeCode <- codeForId outNodeId `catch` (\(_e::SomeASTException) -> return "unknown code")
    inNodeCode  <- codeForId inNodeId `catch` (\(_e::SomeASTException) -> return "unknown code")
    connectPersistent outPort anyPort `catch` (\(e::SomeASTException) ->
        throwM $ ConnectionException outNodeId outNodeCode outPortId inNodeId inNodeCode inPortId e)

data SelfPortDefaultException = SelfPortDefaultException InPortRef
    deriving (Show)

instance Exception SelfPortDefaultException where
    fromException = astExceptionFromException
    toException = astExceptionToException

getPortDefault :: GraphLocation -> InPortRef -> Empire (Maybe PortDefault)
getPortDefault loc port@(InPortRef  _ (Self : _))             = throwM $ SelfPortDefaultException port
getPortDefault loc (InPortRef (NodeLoc _ nodeId) (Arg x : _)) = withGraph loc $ runASTOp $ flip GraphBuilder.getInPortDefault x =<< GraphUtils.getASTTarget nodeId
getPortDefault loc (InPortRef (NodeLoc _ nodeId) [])          = withGraph loc $ runASTOp $ GraphBuilder.getDefault =<< GraphUtils.getASTTarget nodeId

setPortDefault :: GraphLocation -> InPortRef -> Maybe PortDefault -> Empire ()
setPortDefault loc (InPortRef (NodeLoc _ nodeId) port) (Just val) = do
    withTC loc False $ do
        parsed <- runASTOp $ ASTParse.parsePortDefault val
        runAliasAnalysis
        runASTOp $ case port of
            [] -> makeWhole parsed nodeId
            _  -> makeInternalConnection parsed nodeId port
    resendCode loc
setPortDefault loc port Nothing = do
    withTC loc False $ runASTOp $ disconnectPort port
    resendCode loc

disconnect :: GraphLocation -> InPortRef -> Empire ()
disconnect loc@(GraphLocation file _) port@(InPortRef (NodeLoc _ nid) _) = do
    withTC loc False $ do
        runASTOp $ disconnectPort port
        runAliasAnalysis
    resendCode loc

getNodeMeta :: GraphLocation -> NodeId -> Empire (Maybe NodeMeta)
getNodeMeta loc = withGraph loc . runASTOp . AST.getNodeMeta

getCode :: GraphLocation -> Empire Text
getCode loc@(GraphLocation file _) = do
    code <- withUnit (GraphLocation file (Breadcrumb [])) $ use Graph.code
    return $ Code.removeMarkers code

getBuffer :: FilePath -> Empire Text
getBuffer file = getCode (GraphLocation file (Breadcrumb []))

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
                return $ Map.map (view Graph.funName) funs
            withGraph (functionLocation loc) $ GraphBuilder.decodeBreadcrumbs definitionsIDs crumbs

renameNode :: GraphLocation -> NodeId -> Text -> Empire ()
renameNode loc nid name
    | GraphLocation f (Breadcrumb []) <- loc = do
        let stripped = Text.strip name
        withUnit loc $ do
            _ <- liftIO $ ASTParse.runProperVarParser stripped
            oldName <- use $ Graph.clsFuns . ix nid . Graph.funName
            Graph.clsFuns %= Map.adjust (Graph.funName .~ (Text.unpack stripped)) nid
            runASTOp $ do
                fun     <- ASTRead.getFunByNodeId nid >>= ASTRead.cutThroughDocAndMarked
                IR.matchExpr fun $ \case
                    IR.ASGRootedFunction n _ -> flip ASTModify.renameVar (convert stripped) =<< IR.source n
        withGraph (GraphLocation f (Breadcrumb [Breadcrumb.Definition nid])) $ runASTOp $ do
            self <- use $ Graph.breadcrumbHierarchy . BH.self
            v    <- ASTRead.getVarNode self
            ASTModify.renameVar v $ convert stripped
            Code.replaceAllUses v stripped
        resendCode loc
    | otherwise = do
        withTC loc False $ do
            runASTOp $ do
                _        <- liftIO $ ASTParse.runProperPatternParser name
                pat      <- ASTParse.parsePattern name
                Code.propagateLengths pat
                ASTBuilder.ensureNodeHasName generateNodeName nid
                v        <- ASTRead.getASTVar nid
                patIsVar <- ASTRead.isVar pat
                varIsVar <- ASTRead.isVar v
                if patIsVar && varIsVar then do
                    ASTModify.renameVar v $ convert name
                    Code.replaceAllUses v name
                    IR.deleteSubtree pat
                else do
                    ref      <- ASTRead.getASTPointer nid
                    Just beg <- Code.getOffsetRelativeToFile ref
                    varLen   <- IR.getLayer @SpanLength v
                    patLen   <- IR.getLayer @SpanLength pat
                    vEdge    <- ASTRead.getVarEdge nid
                    IR.replaceSource pat vEdge
                    Code.gossipLengthsChangedBy (patLen - varLen) ref
                    void $ Code.applyDiff beg (beg + varLen) name
            runAliasAnalysis
        resendCode loc

dumpGraphViz :: GraphLocation -> Empire ()
dumpGraphViz loc = withGraph loc $ return ()

autolayoutNodesAST :: GraphOp m => [NodeId] -> m ()
autolayoutNodesAST nids = timeIt "autolayoutNodes" $ do
    nodes <- GraphBuilder.buildNodesForAutolayout <!!> "buildNodesForAutolayout"
    conns <- GraphBuilder.buildConnections        <!!> "buildConnections"
    let autolayout = Autolayout.autolayoutNodes nids nodes conns
    traverse_ (uncurry setNodePositionAST) autolayout <!!> "setNodePositionsAST"

autolayoutNodesCls :: ClassOp m => [NodeId] -> m ()
autolayoutNodesCls nids = do
    nodes <- GraphBuilder.buildNodesForAutolayoutCls
    let autolayout = Autolayout.autolayoutNodes nids nodes []
    traverse_ (uncurry setNodePositionCls) autolayout

autolayoutNodes :: GraphLocation -> [NodeId] -> Empire ()
autolayoutNodes loc nids = withGraph' loc (runASTOp $ autolayoutNodesAST nids) (runASTOp $ autolayoutNodesCls nids)

openFile :: FilePath -> Empire ()
openFile path = do
    code <- liftIO (Text.readFile path) <!!> "readFile"
    Library.createLibrary Nothing path  <!!> "createLibrary"
    let loc = GraphLocation path $ Breadcrumb []
    withUnit loc (Graph.code .= code)
    result <- try $ do
        loadCode loc code <!!> "loadCode"
        withUnit loc $ Graph.clsParseError .= Nothing
    case result of
        Left e -> withUnit loc $ Graph.clsParseError ?= e
        _      -> return ()

typecheck :: GraphLocation -> Empire ()
typecheck loc = withTC' loc False (return ()) (return ())

breakDiffs :: [Diff] -> [Diff]
breakDiffs diffs = go [] diffs
    where
        go acc [] = reverse acc
        go acc (d@(Diff range code cursor):list) =
            case Text.span isSpace code of
                (prefix, suffix)
                    | Text.null prefix -> go (d:acc) list
                    | otherwise        -> let rangeEnd   = fromJust (fmap snd range)
                                              newRange   = Just (rangeEnd, rangeEnd)
                                              whitespace = Diff range prefix cursor
                                              onlyCode   = Diff newRange suffix cursor
                                          in go (onlyCode:whitespace:acc) list

substituteCodeFromPoints :: FilePath -> [Diff] -> Empire ()
substituteCodeFromPoints path (breakDiffs -> diffs) = do
    let loc = GraphLocation path (Breadcrumb [])
    changes <- withUnit loc $ do
        oldCode   <- use Graph.code
        let noMarkers    = Code.removeMarkers oldCode
            deltas       = map (\(Diff range code cursor) -> case range of
                Just (start, end) -> (Code.pointToDelta start noMarkers, Code.pointToDelta end noMarkers, code)
                _                 -> (0, fromIntegral (Text.length noMarkers), code))
                        diffs
            viewToReal c = case Text.uncons c of
                Nothing        -> Code.viewDeltasToReal
                Just (char, _) -> if isSpace char then Code.viewDeltasToRealBeforeMarker else Code.viewDeltasToReal
            realDeltas   = map (\(a,b,c) -> let (a', b') = (viewToReal c) oldCode (a,b) in (a', b', c)) deltas
        return realDeltas
    substituteCode path changes

substituteCode :: FilePath -> [(Delta, Delta, Text)] -> Empire ()
substituteCode path changes = do
    let loc = GraphLocation path (Breadcrumb [])
    newCode <- withUnit loc $ Code.applyMany changes
    handle (\(e :: SomeException) -> withUnit loc $ Graph.clsParseError ?= e) $ do
        withUnit loc $ do
            Graph.code .= newCode
        reloadCode loc newCode

lamItemToMapping :: ((NodeId, Maybe Int), BH.LamItem) -> ((NodeId, Maybe Int), (NodeId, NodeId))
lamItemToMapping (idArg, BH.LamItem portMapping _ _) = (idArg, portMapping)

extractMarkedMetasAndIds :: ASTOp g m => NodeRef -> m [(Word64, (Maybe NodeMeta, Maybe NodeId))]
extractMarkedMetasAndIds root = IR.matchExpr root $ \case
    IR.Marked m e -> do
        meta   <- AST.readMeta root
        marker <- getMarker =<< IR.source m
        expr   <- IR.source e
        rest   <- extractMarkedMetasAndIds expr
        nid    <- ASTRead.getNodeId  expr
        nid2   <- ASTRead.getNodeId  root
        return $ (marker, (meta, nid <|> nid2)) : rest
    _ -> concat <$> (mapM (extractMarkedMetasAndIds <=< IR.source) =<< IR.inputs root)

prepareNodeCache :: GraphLocation -> Empire NodeCache
prepareNodeCache loc@(GraphLocation file _) = do
    (funs, topMarkers) <- withUnit (GraphLocation file (Breadcrumb [])) $ do
        funs       <- use Graph.clsFuns
        topMarkers <- runASTOp $ extractMarkedMetasAndIds =<< use Graph.clsClass
        return (Map.keys funs, Map.fromList topMarkers)
    oldMetasAndIds <- forM funs $ \fun -> withGraph (GraphLocation file (Breadcrumb [Breadcrumb.Definition fun])) $ runASTOp $ do
        root       <- ASTRead.getCurrentBody
        oldMetas   <- extractMarkedMetasAndIds root
        return $ Map.fromList oldMetas
    previousPortMappings <- forM funs $ \fun -> withGraph (GraphLocation file (Breadcrumb [Breadcrumb.Definition fun])) $ runASTOp $ do
        hierarchy <- use Graph.breadcrumbHierarchy
        let lamItems = BH.getLamItems hierarchy
            elems    = map lamItemToMapping lamItems
        return $ Map.fromList elems
    let previousNodeIds   = Map.unions $ (Map.mapMaybe snd topMarkers) : (Map.mapMaybe snd <$> oldMetasAndIds)
        previousNodeMetas = Map.unions $ (Map.mapMaybe fst topMarkers) : (Map.mapMaybe fst <$> oldMetasAndIds)
    return $ NodeCache previousNodeIds previousNodeMetas (Map.unions previousPortMappings)    

reloadCode :: GraphLocation -> Text -> Empire ()
reloadCode loc@(GraphLocation file _) code = do
    nodeCache <- prepareNodeCache loc
    withUnit (GraphLocation file (Breadcrumb [])) $ Graph.nodeCache .= nodeCache
    loadCode loc code

putIntoHierarchy :: GraphOp m => NodeId -> NodeRef -> m ()
putIntoHierarchy nodeId marked = do
    let nodeItem = BH.ExprItem Map.empty marked
    Graph.breadcrumbHierarchy . BH.children . at nodeId ?= BH.ExprChild nodeItem

putChildrenIntoHierarchy :: GraphOp m => NodeId -> NodeRef -> m ()
putChildrenIntoHierarchy uuid expr = do
    target       <- ASTRead.getASTTarget uuid
    marked       <- ASTRead.getASTRef uuid
    item         <- prepareChild marked target
    Graph.breadcrumbHierarchy . BH.children . ix uuid .= item

copyMeta :: GraphOp m => NodeRef -> NodeRef -> m ()
copyMeta donor recipient = do
    meta <- AST.readMeta donor
    for_ meta $ AST.writeMeta recipient

markNode :: GraphOp m => NodeId -> m ()
markNode nodeId = do
    var <- ASTRead.getASTMarkerPosition nodeId
    ASTBuilder.attachNodeMarkers nodeId [] var

stripMetadata :: Text -> Text
stripMetadata text = if lexerStream == code then text else flip Text.append "\n" $ Text.stripEnd $ convert withoutMeta
    where
        lexerStream  = Lexer.evalDefLexer (convert text)
        code         = takeWhile (\(Lexer.Token _ _ s) -> isNothing $ Lexer.matchMetadata s) lexerStream
        textTree     = SpanTree.buildSpanTree (convert text) $ code ++ [Lexer.Token 0 0 Lexer.ETX]
        withoutMeta  = SpanTree.foldlSpans (\t (Spanned _ t1) -> t <> t1) "" textTree

removeMetadataNode :: ClassOp m => m ()
removeMetadataNode = do
    unit   <- use Graph.clsClass
    klass' <- ASTRead.classFromUnit unit
    newLinks <- IR.matchExpr klass' $ \case
        IR.ClsASG _ _ _ _ funs -> do
            funs' <- mapM IR.source funs
            catMaybes <$> forM funs (\f -> do
                f' <- IR.source f
                IR.matchExpr f' $ \case
                    IR.Metadata{} -> return Nothing
                    _             -> return (Just f))
    Just (klass'' :: IR.Expr (IR.ClsASG)) <- IR.narrow klass'
    IR.modifyExprTerm klass'' $ wrapped . IR.termClsASG_decls .~ (map IR.unsafeGeneralize newLinks :: [IR.Link (IR.Expr IR.Draft) (IR.Expr Term.ClsASG)])

getNextTopLevelMarker :: ClassOp m => m Word64
getNextTopLevelMarker = do
    globalMarkers <- use Graph.clsCodeMarkers
    let highestIndex = Safe.maximumMay $ Map.keys globalMarkers
        newMarker    = maybe 0 succ highestIndex
    Code.invalidateMarker newMarker
    return newMarker

getASGRootedFunctionLink :: ClassOp m => EdgeRef -> m EdgeRef
getASGRootedFunctionLink link = do
    ref <- IR.source link
    IR.matchExpr ref $ \case
        IR.Documented _d e -> return e
        IR.Marked     _m e -> return e
        _                  -> return link

markFunctions :: ClassOp m => NodeRef -> m ()
markFunctions unit = do
    klass' <- ASTRead.classFromUnit unit
    IR.matchExpr klass' $ \case
        IR.ClsASG _ _ _ _ funs -> do
            forM_ funs $ \fun -> IR.source fun >>= \asgFun -> ASTRead.cutThroughDoc asgFun >>= \f -> IR.matchExpr f $ \case
                IR.Marked{}            -> return ()
                IR.ASGRootedFunction{} -> do
                    newMarker <- getNextTopLevelMarker
                    funStart  <- Code.functionBlockStartRef f
                    Code.insertAt funStart (Code.makeMarker newMarker)
                    marker    <- IR.marker' newMarker
                    markedFun <- IR.marked' marker f
                    Graph.clsCodeMarkers . at newMarker ?= markedFun
                    LeftSpacedSpan (SpacedSpan off prevLen) <- view CodeSpan.realSpan <$> IR.getLayer @CodeSpan f
                    let markerLength = convert $ Text.length $ Code.makeMarker newMarker
                    IR.putLayer @CodeSpan marker $ CodeSpan.mkRealSpan (LeftSpacedSpan (SpacedSpan 0 markerLength))
                    IR.putLayer @CodeSpan markedFun $ CodeSpan.mkRealSpan (LeftSpacedSpan (SpacedSpan off prevLen))
                    IR.putLayer @CodeSpan f $ CodeSpan.mkRealSpan (LeftSpacedSpan (SpacedSpan 0 prevLen))
                    asgLink <- getASGRootedFunctionLink fun
                    IR.replaceSource markedFun asgLink
                    Code.gossipLengthsChangedByCls markerLength markedFun
                _ -> return ()

loadCode :: GraphLocation -> Text -> Empire ()
loadCode (GraphLocation file _) code = do
    let loc = GraphLocation file def
    (unit, IR.Rooted ir ref, exprMap) <- liftIO $ ASTParse.runProperParser code
    activeFiles . at file . traverse . Library.body . Graph.clsClass .= unit
    activeFiles . at file . traverse . Library.body . Graph.clsCodeMarkers .= (coerce exprMap)
    activeFiles . at file . traverse . Library.body . Graph.code .= code
    activeFiles . at file . traverse . Library.body . Graph.clsFuns .= Map.empty
    (codeHadMeta, prevParseError) <- withUnit (GraphLocation file (Breadcrumb [])) $ do
        prevParseError <- use $ Graph.clsParseError
        Graph.clsParseError .= Nothing
        putNewIRCls ir
        FileMetadata fileMetadata <- runASTOp readMetadata'
        let savedNodeMetas = Map.fromList $ map (\(MarkerNodeMeta m meta) -> (m, meta)) fileMetadata
        Graph.nodeCache . nodeMetaMap %= (\cache -> Map.union cache savedNodeMetas)
        runASTOp $ do
            let codeWithoutMeta = stripMetadata code
            Graph.code .= codeWithoutMeta
            metaRef  <- ASTRead.getMetadataRef unit
            removeMetadataNode
            forM_ metaRef IR.deepDelete
            return (codeWithoutMeta /= code, prevParseError)
    when (codeHadMeta && isJust prevParseError) $ resendCode loc
    functions <- withUnit loc $ do
        klass <- use Graph.clsClass
        runASTOp $ do
            markFunctions klass
            funs <- ASTRead.classFunctions klass
            forM funs $ \f -> ASTRead.cutThroughDoc f >>= \fun -> IR.matchExpr fun $ \case
                IR.Marked m _e -> do
                    marker <- getMarker =<< IR.source m
                    uuid   <- use $ Graph.nodeCache . nodeIdMap . at marker
                    return (uuid, f)
    for_ functions $ \(lastUUID, fun) -> do
        uuid <- Library.withLibrary file (fst <$> makeGraph fun lastUUID)
        let loc' = GraphLocation file $ Breadcrumb [Breadcrumb.Definition uuid]
        autolayout loc'
    autolayoutTopLevel loc
    return ()

infixl 5 |>
(|>) :: GraphLocation -> BreadcrumbItem -> GraphLocation
(|>) (GraphLocation file bc) item = GraphLocation file $ coerce $ (<> [item]) $ coerce bc

autolayout :: GraphLocation -> Empire ()
autolayout loc = do
    kids <- withGraph loc $ runASTOp $ do
        kids <- uses Graph.breadcrumbHierarchy (view BH.children)
        needLayout <- fmap catMaybes $ forM (Map.keys kids) $ \id -> do
            meta <- AST.getNodeMeta id
            return $ if meta /= def then Nothing else Just id
        autolayoutNodesAST needLayout
        return kids
    let next = concatMap (\(k, v) -> case v of
            BH.LambdaChild{}                -> [Breadcrumb.Lambda k]
            BH.ExprChild (BH.ExprItem pc _) -> map (Breadcrumb.Arg k) (Map.keys pc)) $ Map.assocs kids
    traverse_ (\a -> autolayout (loc |> a)) next

getNodeMetas :: GraphLocation -> [NodeLoc] -> Empire [Maybe (NodeLoc, NodeMeta)]
getNodeMetas loc nids
    | GraphLocation f (Breadcrumb []) <- loc = withUnit loc $ runASTOp $ do
        clsFuns    <- use Graph.clsFuns
        forM (Map.assocs clsFuns) $ \(id, fun) -> do
            case find (\n -> convert n == id) nids of
                Just nl -> do
                    f     <- ASTRead.getFunByNodeId id
                    fmap (nl,) <$> AST.readMeta f
                _       -> return Nothing
    | otherwise = withGraph loc $ runASTOp $ do
        kids <- uses Graph.breadcrumbHierarchy (view BH.children)
        forM (Map.keys kids) $ \id -> do
            case find (\n -> convert n == id) nids of
                Just nl -> do
                    fmap (nl,) <$> AST.getNodeMeta id
                _       -> return Nothing

autolayoutTopLevel :: GraphLocation -> Empire ()
autolayoutTopLevel loc = do
    withUnit loc $ runASTOp $ do
        clsFuns    <- use Graph.clsFuns
        needLayout <- fmap catMaybes $ forM (Map.assocs clsFuns) $ \(id, fun) -> do
            f    <- ASTRead.getFunByNodeId id
            meta <- AST.readMeta f
            let fileOffset = fun ^. Graph.funGraph . Graph.fileOffset
            return $ if meta /= def then Nothing else Just (id, fileOffset)

        let sortedNeedLayout = sortOn snd needLayout
            positions  = map (Position.fromTuple . (,0)) [0,gapBetweenNodes..]
            autolayout = zipWith (\(id, _) pos -> (id,pos)) sortedNeedLayout positions
        traverse_ (uncurry setNodePositionCls) autolayout

printMarkedExpression :: GraphOp m => NodeRef -> m Text
printMarkedExpression ref = do
    exprMap <- getExprMap
    realRef <- IR.matchExpr ref $ \case
        IR.Marked _m expr -> IR.source expr
        _                 -> return ref
    expr    <- Text.pack <$> ASTPrint.printExpression realRef
    let markers = Map.keys $ Map.filter (== ref) exprMap
        marker  = case markers of
            (index:_) -> Text.pack $ "«" <> show index <> "»"
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
        ref      = Map.lookup (fromIntegral index) exprMap'
    case ref of
        Nothing -> return Nothing
        Just r  -> IR.matchExpr r $ \case
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
        root       <- ASTRead.getCurrentBody
        oldMetas   <- extractMarkedMetasAndIds root
        return [ MarkerNodeMeta marker meta | (marker, (Just meta, _)) <- oldMetas ]
    return $ concat metas

addMetadataToCode :: FilePath -> Empire Text
addMetadataToCode file = do
    metadata <- FileMetadata <$> dumpMetadata file
    let metadataJSON           = (TL.toStrict . Aeson.encodeToLazyText . Aeson.toJSON) metadata
        metadataJSONWithHeader = Lexer.mkMetadata (Text.cons ' ' metadataJSON)
    withUnit (GraphLocation file (Breadcrumb [])) $ do
        code <- use Graph.code
        let metadataJSONWithHeaderAndOffset = Text.cons '\n' metadataJSONWithHeader
        return $ Text.concat [code, metadataJSONWithHeaderAndOffset]

parseMetadata :: MonadIO m => Text -> m FileMetadata
parseMetadata meta =
    let json = Text.drop (Text.length "### META ") meta
        metadata = Aeson.eitherDecodeStrict' $ Text.encodeUtf8 json
    in case metadata of
        Right fm  -> return fm
        Left  err -> return (FileMetadata [])

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

insertCodeBeforeFunction :: GraphLocation -> Text -> Empire Text
insertCodeBeforeFunction loc@(GraphLocation file _) codeToInsert = do
    let nodeId = topLevelFunctionID loc
    withUnit (GraphLocation file def) $ runASTOp $ do
        ref <- ASTRead.getFunByNodeId nodeId
        fo <- Code.functionBlockStartRef ref
        Code.insertAt fo (Text.snoc (Text.snoc codeToInsert '\n') '\n')

insertCodeBetween :: GraphOp m => [NodeId] -> [NodeId] -> Text -> m Text
insertCodeBetween beforeNodes afterNodes codeToInsert = do
    beforeRefs <- fmap Set.fromList $ forM beforeNodes ASTRead.getASTRef
    afterRefs  <- fmap Set.fromList $ forM afterNodes  ASTRead.getASTRef
    topSeq     <- ASTRead.getCurrentBody
    output     <- getCurrentFunctionOutput
    refToInsertAfter <- findRefToInsertAfter (Set.insert output beforeRefs) afterRefs topSeq
    insertPos        <- case refToInsertAfter of
        Nothing -> do
            len <- IR.getLayer @SpanLength output
            (+len) <$> Code.getCurrentBlockBeginning
        Just r  -> do
            Just beg <- Code.getOffsetRelativeToFile r
            len      <- IR.getLayer @SpanLength r
            return $ beg + len
    Code.insertAt insertPos codeToInsert

generateCollapsedDefCode :: GraphOp m => Text -> [OutPortRef] -> [OutPortRef] -> [NodeId] -> m (Text, Text, Maybe Text, Position)
generateCollapsedDefCode defName inputs outputs bodyIds = do
    (inputSidebar, _) <- GraphBuilder.getEdgePortMapping
    inputNames <- fmap (map (view _2) . sortOn fst) $ forM inputs $ \(OutPortRef (NodeLoc _ nodeId) pid) -> do
        position <- if nodeId == inputSidebar then return def
                                              else fmap (view NodeMeta.position) <$> AST.getNodeMeta nodeId
        name     <- ASTRead.getASTOutForPort nodeId pid >>= ASTRead.getVarName
        return (position, name)
    outputNames <- forM outputs $ \(OutPortRef (NodeLoc _ nodeId) pid) ->
        ASTRead.getASTOutForPort nodeId pid >>= ASTRead.getVarName
    let singleOutput = case outputs of
            [a] -> Just $ a ^. PortRef.srcNodeId
            _   -> Nothing
    singleOutputMeta <- forM singleOutput AST.getNodeMeta
    let singleOutputPosition = fmap (view NodeMeta.position) (join singleOutputMeta)
    bodyMetas <- mapM AST.getNodeMeta bodyIds
    let bodyPositions = sortOn (view Position.x) $ map (view NodeMeta.position) $ catMaybes bodyMetas
        rightmostPosition = last bodyPositions
    codeBegs <- fmap (sortOn fst) $ forM bodyIds $ \nid -> do
        ref     <- ASTRead.getASTPointer nid
        Just cb <- Code.getOffsetRelativeToFile ref
        return (cb, ref)
    currentIndentation <- Code.getCurrentIndentationLength
    let indentBy i l = "\n" <> Text.replicate (fromIntegral i) " " <> l
        bodyIndented = indentBy Code.defaultIndentationLength
        retIndented  = indentBy currentIndentation
    newCodeBlockBody <- fmap Text.concat $ forM codeBegs $ \(beg, ref) -> do
        len  <- IR.getLayer @SpanLength ref
        code <- Code.getAt beg (beg + len)
        return $ bodyIndented code
    let header =  "def "
               <> Text.unwords (defName : fmap convert inputNames)
               <> ":"
    returnBody <- case outputNames of
        []  -> do
            let lastNode = snd $ unsafeLast codeBegs
            handle (\(e::NotUnifyException) -> return Nothing) (Just <$> (ASTRead.getVarNode lastNode >>= Code.getCodeOf))
        [a] -> return $ Just $ convert a
        _   -> return $ Just $ "(" <> Text.intercalate ", " (convert <$> outputNames) <> ")"
    let returnLine = case returnBody of
            Just n -> bodyIndented n
            _      -> ""
    let defCode = header <> newCodeBlockBody <> returnLine
    let useLine = retIndented $ maybe "" (<> " = ") returnBody
                              <> Text.unwords (defName : fmap convert inputNames)
    return (defCode, useLine, returnBody, fromMaybe rightmostPosition singleOutputPosition)

topLevelFunctionID :: GraphLocation -> NodeId
topLevelFunctionID (GraphLocation _ (Breadcrumb (Breadcrumb.Definition nodeId:_))) = nodeId

collapseToFunction :: GraphLocation -> [NodeId] -> Empire ()
collapseToFunction loc@(GraphLocation file _) nids = do
    nodes <- getNodes (GraphLocation file def)
    when (null nids) $ throwM ImpossibleToCollapse
    let names   = Set.fromList $ mapMaybe (view Node.name) nodes
        newName = generateNewFunctionName names "func"
    (defCode, useVarName, outputPosition) <- withGraph loc $ runASTOp $ do
        let ids = Set.fromList nids
        connections       <- GraphBuilder.buildConnections
        (inputSidebar, _) <- GraphBuilder.getEdgePortMapping
        let srcInIds = flip Set.member ids . view PortRef.srcNodeId . fst
            dstInIds = flip Set.member ids . view PortRef.dstNodeId . snd
            inConns  = filter (\x -> dstInIds x && not (srcInIds x)) connections
            inputs   = nub $ fst <$> inConns
            outConns = filter (\x -> srcInIds x && not (dstInIds x)) connections
        outConns' <- filterM (\a -> not <$> isOutput (view PortRef.dstNodeId $ snd a)) outConns
        let outputs  = nub $ fst <$> outConns'
            useSites = outConns' ^.. traverse . _2 . PortRef.dstNodeId
        (defCode, useCode, useVarName, outputPosition) <- generateCollapsedDefCode newName inputs outputs nids
        let inputsNodeIds = List.delete inputSidebar $ map (view PortRef.srcNodeId) inputs
        insertCodeBetween useSites inputsNodeIds useCode
        return (defCode, useVarName, outputPosition)
    code <- insertCodeBeforeFunction loc defCode
    reloadCode  loc code
    typecheckWithRecompute (GraphLocation file def)
    removeNodes loc nids
    withGraph loc $ runASTOp $ do
        nodes <- GraphBuilder.buildNodes
        let funUseNodes = filter (\n -> n ^. Node.name == useVarName) nodes
        case funUseNodes of
            [a] -> setNodePositionAST (a ^. Node.nodeId) outputPosition
            _   -> return ()

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
            refs <- mapM ASTRead.getFunByNodeId [ nid | (nid, Just (view Graph.funName -> _name)) <- names]
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

data B = Break | NoBreak deriving Show

exprBreaker :: [Text] -> [B] -> [Text]
exprBreaker _  []     = []
exprBreaker [] _      = []
exprBreaker (l:ls) bs = go [l] ls bs
    where
        go acc [] _ = reverse acc
        go (acc:accs) (ln:lns) (b:breaks) = case b of
            Break -> go (ln:acc:accs) lns breaks
            NoBreak -> go ((Text.stripEnd $ Text.unlines [acc, ln]):accs) lns breaks

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
paste loc position (Text.pack -> text) = do
    let lexerStream  = Lexer.evalDefLexer (convert text)
        (_, code)    = partition (\(Lexer.Token _ _ s) -> isJust $ Lexer.matchMetadata s) lexerStream
        textTree     = SpanTree.buildSpanTree (convert text) code
        withoutMeta  = SpanTree.foldlSpans (\t (Spanned _ t1) -> t <> t1) "" textTree
        metaLine     = Text.drop (Text.length $ convert withoutMeta) text
    withTC loc False $ do
        fm          <- forM (Safe.headMay [metaLine]) parseMetadata
        let metas   =  maybe [] (\(FileMetadata fm') -> moveToOrigin fm') fm
        indentation <- fromIntegral <$> runASTOp Code.getCurrentIndentationLength
        let exprs' = Text.lines $ convert withoutMeta
            cut    = snd $ foldl' (\(initialIndent, acc) e -> let indent' = Text.length (Text.takeWhile isSeparator e) in (initialIndent, if indent' <= initialIndent then Break:acc else NoBreak:acc)) (Text.length (Text.takeWhile isSeparator (head exprs')), []) exprs'
        forM (exprBreaker exprs' cut) $ \(Text.strip -> expr) -> do
            let (marker, rest) = Text.breakOn "»" expr & both %~ Text.drop 1
                mark           = Safe.readMay (Text.unpack marker) :: Maybe Word64
                newMeta        = case mark of
                    Just marker -> fromMaybe def $ fmap meta $ find (\(MarkerNodeMeta m me) -> m == marker) metas
                    _           -> def
                movedMeta      = newMeta & NodeMeta.position %~ Position.move (coerce position)
                code           = if Text.null rest then expr else rest
                indented       = indent indentation code
                nodeCode       = Code.removeMarkers indented
            when (not $ Text.null expr) $ do
                uuid <- liftIO UUID.nextRandom
                void $ addNodeNoTC loc uuid nodeCode Nothing movedMeta
    autolayout loc
    resendCode loc

includeWhitespace :: Text -> (Delta, Delta) -> (Delta, Delta)
includeWhitespace c (s, e) = (newStart, e)
    where
        prefix = Text.take (fromIntegral s) c
        whitespaceBefore = Text.takeWhileEnd isSeparator prefix
        newStart = s - fromIntegral (if Text.length whitespaceBefore `rem` 4 == 0 then Text.length whitespaceBefore else 0)

copyText :: GraphLocation -> [Range] -> Empire Text
copyText loc@(GraphLocation file _) ranges = do
    allMetadata <- dumpMetadata file
    withUnit (GraphLocation file (Breadcrumb [])) $ do
        oldCode <- use Graph.code
        let markedRanges = map (includeWhitespace oldCode . rangeToMarked oldCode) ranges
        codes <- forM markedRanges $ uncurry Code.getAt
        let code                   = Text.concat codes
            markers                = Code.extractMarkers code
            relevantMetadata       = filter (\(MarkerNodeMeta marker _) -> marker `Set.member` markers) allMetadata
            metadata               = FileMetadata relevantMetadata
            metadataJSON           = (TL.toStrict . Aeson.encodeToLazyText . Aeson.toJSON) metadata
            metadataJSONWithHeader = Lexer.mkMetadata (Text.cons ' ' metadataJSON)
            clipboard              = Text.concat [code, metadataJSONWithHeader]
        return clipboard

remarkerSnippet :: ClassOp m => [MarkerNodeMeta] -> Text -> m (Text, [MarkerNodeMeta])
remarkerSnippet pastedMeta code = do
    oldCode <- use Graph.code
    let reservedMarkers             = Code.extractMarkers oldCode
        (pastedCode, substitutions) = Code.remarkerCode code reservedMarkers
        updatedMeta = mapMaybe (\(MarkerNodeMeta marker meta) -> case Map.lookup marker substitutions of
            Just new -> Just (MarkerNodeMeta new meta)
            Nothing -> Nothing) pastedMeta
    return (pastedCode, updatedMeta)

rangeToSpan :: Range -> (Delta, Delta)
rangeToSpan (Range s e) = (fromIntegral s, fromIntegral e)

rangeToMarked :: Text -> Range -> (Delta, Delta)
rangeToMarked code range = (start, end)
    where
        span         = rangeToSpan range
        (start, end) = Code.viewDeltasToRealBeforeMarker code span

pasteText :: GraphLocation -> [Range] -> [Text] -> Empire Text
pasteText loc@(GraphLocation file _) ranges (Text.concat -> text) = do
    let lexerStream  = Lexer.evalDefLexer (convert text)
        (meta, code) = partition (\(Lexer.Token _ _ s) -> isJust $ Lexer.matchMetadata s) lexerStream
        textTree     = SpanTree.buildSpanTree (convert text) code
        withoutMeta  = SpanTree.foldlSpans (\t (Spanned _ t1) -> t <> t1) "" textTree
        metaLine     = Text.drop (Text.length $ convert withoutMeta) text
    (code, updatedMeta, cursors) <- withUnit (GraphLocation file (Breadcrumb [])) $ do
        code <- use Graph.code
        let markedRanges   = map ({-includeWhitespace code .-} rangeToMarked code) ranges
            rangesReversed = reverse $ sortOn (view _1) markedRanges
        FileMetadata m <- parseMetadata metaLine
        (updatedMetas, cursors) <- unzip <$> (runASTOp $ forM rangesReversed $ \(start, end) -> do
            (snippet, metas) <- remarkerSnippet m $ convert withoutMeta
            Code.applyDiff start end snippet
            code' <- use Graph.code
            let endPosition = start + fromIntegral (Text.length snippet)
                cursorPos   = Code.deltaToPoint endPosition code'
            return (metas, cursorPos))
        code           <- use Graph.code
        return (code, concat updatedMetas, cursors)
    reloadCode (GraphLocation file (Breadcrumb [])) code `catch` \(e::ASTParse.SomeParserException) ->
        withUnit (GraphLocation file (Breadcrumb [])) (Graph.code .= code >> Graph.clsParseError ?= toException e)
    funs <- withUnit (GraphLocation file (Breadcrumb [])) $ do
        funs <- use Graph.clsFuns
        return $ Map.keys funs
    forM funs $ \fun -> withGraph (GraphLocation file (Breadcrumb [Breadcrumb.Definition fun])) $ runASTOp $ do
        forM updatedMeta $ \(MarkerNodeMeta marker meta) -> do
            nodeid <- getNodeIdForMarker $ fromIntegral marker
            forM nodeid $ \nid -> setNodeMetaGraph nid meta
    resendCodeWithCursor (GraphLocation file (Breadcrumb [])) (Safe.lastMay cursors)
    return code

nativeModuleName :: Text
nativeModuleName = "Native"

getImportsInFile :: ClassOp m => m [Text]
getImportsInFile = do
    unit <- use Graph.clsClass
    IR.matchExpr unit $ \case
        IR.Unit imps _ _ -> do
            imps' <- IR.source imps
            IR.matchExpr imps' $ \case
                IR.UnresolvedImportHub imps -> forM imps $ \imp -> do
                    imp' <- IR.source imp
                    IR.matchExpr imp' $ \case
                        IR.UnresolvedImport a _ -> do
                            a' <- IR.source a
                            IR.matchExpr a' $ \case
                                IR.UnresolvedImportSrc n -> case n of
                                    Term.Absolute n -> return $ convert n
        IR.ClsASG{} -> return [] -- why does it put ClsASG and not Unit when file does not parse???

getAvailableImports :: GraphLocation -> Empire [ImportName]
getAvailableImports (GraphLocation file _) = withUnit (GraphLocation file (Breadcrumb [])) $ do
    explicitImports <- runASTOp getImportsInFile
    let implicitImports = Set.fromList [nativeModuleName, "Std.Base"]
        resultSet       = Set.fromList explicitImports `Set.union` implicitImports
    return $ Set.toList resultSet

classToHints :: IR.Class -> ClassHints
classToHints (IR.Class constructors methods) = ClassHints cons' meth'
    where
        getDoc = maybe def convert . view IR.documentation
        cons'  = ((, def) . convert) <$> Map.keys constructors
        meth'  = (convert *** getDoc) <$> (filter (isPublicMethod . fst) $ Map.toList methods)

isPublicMethod :: IR.Name -> Bool
isPublicMethod (nameToString -> n) = Safe.headMay n /= Just '_'

importsToHints :: Module.Imports -> ModuleHints
importsToHints (Module.Imports classes functions) = ModuleHints funHints classHints
    where
        getDoc     = maybe def convert . view IR.documentation
        funHints   = (convert *** getDoc) <$> (filter (isPublicMethod . fst) $ Map.toList functions)
        classes'   = Map.mapKeys convert classes
        classHints = classToHints . view IR.documentedItem <$> classes'

data ModuleCompilationException = ModuleCompilationException Compilation.ModuleCompilationError
    deriving (Show)

instance Exception ModuleCompilationException where
    toException = astExceptionToException
    fromException = astExceptionFromException

filterPrimMethods :: Module.Imports -> Module.Imports
filterPrimMethods (Module.Imports classes funs) = Module.Imports classes properFuns
    where
        properFuns = Map.filterWithKey (\k _ -> not $ isPrimMethod k) funs
        isPrimMethod (nameToString -> n) = "prim" `List.isPrefixOf` n || n == "#uminus#"

qualNameToText :: QualName -> Text
qualNameToText = convert

getImports :: GraphLocation -> [Text] -> Empire ImportsHints
getImports loc _ = getSearcherHints loc

getImportPaths :: GraphLocation -> IO (Map.Map IR.Name FilePath)
getImportPaths (GraphLocation file _) = do
    lunaroot        <- liftIO $ canonicalizePath =<< getEnv "LUNAROOT"
    currentProjPath <- liftIO $ Project.findProjectRootForFile =<< Path.parseAbsFile file
    let importPaths = ("Std", lunaroot <> "/Std/") : ((Project.getProjectName &&& Path.toFilePath) <$> maybeToList currentProjPath)
    return $ Map.fromList importPaths

getSearcherHints :: GraphLocation -> Empire ImportsHints
getSearcherHints (GraphLocation file _) = do
    currentProjPath <- liftIO $ Project.findProjectRootForFile =<< Path.parseAbsFile file
    projectSources  <- liftIO $ case currentProjPath of
        Nothing -> return []
        Just p  -> Bimap.elems <$> Project.findProjectSources p
    lunaroot        <- liftIO $ canonicalizePath =<< getEnv "LUNAROOT"
    lunaRootSources <- liftIO $ Project.findProjectSources =<< Path.parseAbsDir (lunaroot <> "/Std")
    let stdModules   = map ((Text.append "Std.") . qualNameToText) $ Bimap.elems lunaRootSources
    let importPaths = ("Std", lunaroot <> "/Std/") : ((Project.getProjectName &&& Path.toFilePath) <$> maybeToList currentProjPath)
    importsMVar     <- view modules
    cmpModules  <- liftIO $ readMVar importsMVar
    std         <- liftIO $ Compilation.requestModules (Map.fromList importPaths) (map convert stdModules) cmpModules
    proj        <- liftIO $ Compilation.requestModules (Map.fromList importPaths) projectSources cmpModules
    stdImports  <- either (\e -> liftIO (print e) >> return def) (return . fst) std
    projImports <- either (\e -> liftIO (print e) >> return def) (return . fst) std
    let allImports = Map.union stdImports projImports
    return $ Map.fromList $ map (\(a, b) -> (qualNameToText a, importsToHints b)) $ Map.toList allImports

setInterpreterState :: Interpreter.Request -> Empire ()
setInterpreterState (Interpreter.Start loc) = do
    activeInterpreter .= True
    Publisher.notifyInterpreterUpdate "Interpreter running"
    runInterpreter loc
setInterpreterState (Interpreter.Pause loc) = do
    activeInterpreter .= False
    Publisher.notifyInterpreterUpdate "Interpreter stopped"
    withTC' loc False (return ()) (return ())
setInterpreterState (Interpreter.Reload loc) = do
    runInterpreter loc

-- internal

getName :: GraphLocation -> NodeId -> Empire (Maybe Text)
getName loc nid = withGraph' loc (runASTOp $ GraphBuilder.getNodeName nid) $ use (Graph.clsFuns . ix nid . Graph.funName . packed . re _Just)

generateNodeName :: GraphOp m => NodeRef -> m Text
generateNodeName = ASTPrint.genNodeBaseName >=> generateNodeNameFromBase

generateNodeNameFromBase :: GraphOp m => Text -> m Text
generateNodeNameFromBase base = do
    ids   <- uses Graph.breadcrumbHierarchy BH.topLevelIDs
    names <- Set.fromList . catMaybes <$> mapM GraphBuilder.getNodeName ids
    return $ generateNewFunctionName names base

generateNewFunctionName :: Set Text -> Text -> Text
generateNewFunctionName forbiddenNames base =
    let allPossibleNames = zipWith (<>) (repeat base) (convert . show <$> [1..])
        Just newName     = find (not . flip Set.member forbiddenNames) allPossibleNames
    in newName

runTC :: GraphLocation -> Bool -> Bool -> Bool -> Command ClsGraph ()
runTC loc flush interpret recompute = do
    g <- get
    Publisher.requestTC loc g flush interpret recompute

typecheckWithRecompute :: GraphLocation -> Empire ()
typecheckWithRecompute loc@(GraphLocation file _) = do
    withGraph' (GraphLocation file def) (return ()) (runTC loc True True True)

runInterpreter :: GraphLocation -> Empire ()
runInterpreter loc@(GraphLocation file _) = do
    withGraph' (GraphLocation file def) (return ()) (runTC loc True True False)

withTC' :: GraphLocation -> Bool -> Command Graph a -> Command ClsGraph a -> Empire a
withTC' loc@(GraphLocation file bs) flush actG actC = do
    res       <- withGraph' loc actG actC
    interpret <- use activeInterpreter
    withGraph' (GraphLocation file def) (return ()) (runTC loc flush interpret False)
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
    if disconnectOutputEdge then setOutputTo nothing else do
        dstTarget <- ASTRead.getASTTarget dst
        dstBeg    <- Code.getASTTargetBeginning dst
        oldLen    <- IR.getLayer @SpanLength dstTarget
        Code.applyDiff dstBeg (dstBeg + oldLen) nothingExpr
        GraphUtils.rewireNode dst nothing
        let lenDiff = fromIntegral (Text.length nothingExpr) - oldLen
        dstPointer <- ASTRead.getASTPointer dst
        Code.gossipLengthsChangedBy lenDiff dstPointer

removeInternalConnection :: GraphOp m => NodeId -> InPortId -> m ()
removeInternalConnection nodeId port = do
    dstAst <- ASTRead.getTargetEdge nodeId
    beg    <- Code.getASTTargetBeginning nodeId
    ASTBuilder.removeArgument dstAst beg port

makeInternalConnection :: GraphOp m => NodeRef -> NodeId -> InPortId -> m ()
makeInternalConnection srcAst dst inPort = do
    dstBeg <- Code.getASTTargetBeginning dst
    dstAst <- ASTRead.getTargetEdge dst
    ASTBuilder.makeConnection dstAst dstBeg inPort srcAst

makeWhole :: GraphOp m => NodeRef -> NodeId -> m ()
makeWhole srcAst dst = do
    (_, out) <- GraphBuilder.getEdgePortMapping
    let connectToOutputEdge = out == dst
    if connectToOutputEdge then setOutputTo srcAst else do
        dstTarget <- ASTRead.getASTTarget dst
        dstBeg    <- Code.getASTTargetBeginning dst
        oldLen    <- IR.getLayer @SpanLength dstTarget
        newExpr   <- ASTPrint.printFullExpression srcAst
        Code.applyDiff dstBeg (dstBeg + oldLen) newExpr
        GraphUtils.rewireNode dst srcAst
        let lenDiff = fromIntegral (Text.length newExpr) - oldLen
        dstPointer <- ASTRead.getASTPointer dst
        Code.gossipLengthsChangedBy lenDiff dstPointer

prepareGraphError :: SomeException -> ErrorAPI.Error ErrorAPI.GraphError
prepareGraphError e | Just (BH.BreadcrumbDoesNotExistException content) <- fromException e = ErrorAPI.Error ErrorAPI.BreadcrumbDoesNotExist . convert $ show content
                    | otherwise                                                            = ErrorAPI.Error ErrorAPI.OtherGraphError        . convert $ displayException e

prettyException :: Exception e => e -> IO String
prettyException e = do
    stack <- whoCreated e
    return $ displayException e ++ "\n" ++ renderStack stack

prepareLunaError :: SomeException -> IO (ErrorAPI.Error ErrorAPI.LunaError)
prepareLunaError e = case prepareGraphError e of
    ErrorAPI.Error ErrorAPI.OtherGraphError _ -> (ErrorAPI.Error ErrorAPI.OtherLunaError . convert) <$> prettyException e
    ErrorAPI.Error tpe content                -> return $ ErrorAPI.Error (ErrorAPI.Graph tpe) content


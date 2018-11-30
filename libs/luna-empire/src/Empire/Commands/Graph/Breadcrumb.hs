module Empire.Commands.Graph.Breadcrumb where

import Empire.Prelude

import qualified Data.Map                             as Map
import qualified Data.UUID.V4                         as UUID
import qualified Empire.ASTOps.BreadcrumbHierarchy    as ASTBreadcrumb
import qualified Empire.ASTOps.Read                   as ASTRead
import qualified Empire.Data.BreadcrumbHierarchy      as BH
import qualified Empire.Data.Graph                    as Graph
import qualified Empire.Data.Library                  as Library
import qualified Luna.Syntax.Text.Parser.Ast.CodeSpan as CodeSpan

import Control.Exception.Safe          (handle)
import Control.Monad                   (forM)
import Control.Monad.Reader            (ask)
import Control.Monad.State             (get, put)
import Data.Maybe                      (listToMaybe, maybe)
import Data.Text.Span                  (SpacedSpan (SpacedSpan))
import Empire.ASTOp                    (GraphOp, runASTOp, runAliasAnalysis)
import Empire.Commands.Code            (functionBlockStartRef, propagateLengths)
import Empire.Data.AST                 (NodeRef)
import Empire.Data.BreadcrumbHierarchy (navigateTo, replaceAt)
import Empire.Data.Layers              (Marker, SpanLength)
import Empire.Empire                   (Command, runEmpire, zoomCommand)
import LunaStudio.Data.Breadcrumb      (Breadcrumb (Breadcrumb),
                                        BreadcrumbItem (Definition))
import LunaStudio.Data.NodeCache       (portMappingMap)
import LunaStudio.Data.NodeId          (NodeId)
import LunaStudio.Data.PortRef         (OutPortRef (OutPortRef))


makeGraph
    :: NodeRef -> Maybe NodeId-> Command Library.Library (NodeId, Graph.Graph)
makeGraph fun lastUUID = zoomCommand Library.body $ makeGraphCls fun lastUUID

extractMarkers :: NodeRef -> GraphOp [Word64]
extractMarkers root = matchExpr root $ \case
    Marked m _ -> do
        marker <- ASTBreadcrumb.getMarker =<< source m
        pure [marker]
    _ -> do
        ins     <- inputs root
        markers <- mapM (\i -> source i >>= extractMarkers) ins
        pure $ concat markers

makeGraphCls :: NodeRef -> Maybe NodeId
    -> Command Graph.ClsGraph (NodeId, Graph.Graph)
makeGraphCls fun lastUUID = do
    nodeCache <- use $ Graph.userState . Graph.clsNodeCache
    uuid      <- maybe (liftIO UUID.nextRandom) return lastUUID
    (funName, ref, fileOffset) <- runASTOp $ do
        putLayer @Marker fun . Just
            =<< toPortMarker (OutPortRef (convert uuid) mempty)
        asgFun <- ASTRead.cutThroughDocAndMarked fun
        matchExpr asgFun $ \case
            ASGFunction n _ _ -> do
                offset <- functionBlockStartRef asgFun
                name   <- handle (\(_e::ASTRead.InvalidNameException) -> pure "")
                    $ ASTRead.getVarName' =<< source n
                pure (nameToString name, asgFun, offset)
    let oldPortMapping = nodeCache ^. portMappingMap . at (uuid, Nothing)
    portMapping <- fromJustM
        (liftIO $ (,) <$> UUID.nextRandom <*> UUID.nextRandom)
        oldPortMapping
    globalMarkers <- use $ Graph.userState . Graph.clsCodeMarkers
    let bh    = BH.LamItem portMapping ref def
        graph = Graph.Graph bh def globalMarkers def def fileOffset nodeCache
    Graph.userState . Graph.clsFuns . at uuid
        ?= Graph.FunctionGraph funName graph Map.empty
    updatedCache <- withRootedFunction uuid $ do
        runASTOp $ do
            markers <- extractMarkers ref
            let localMarkers = Map.filterWithKey
                    (\k _ -> k `elem` markers)
                    globalMarkers
            Graph.codeMarkers .= localMarkers
            propagateLengths ref
        runAliasAnalysis
        runASTOp $ do
            ASTBreadcrumb.makeTopBreadcrumbHierarchy ref
            ASTBreadcrumb.restorePortMappings uuid (nodeCache ^. portMappingMap)
            use Graph.graphNodeCache
    Graph.userState . Graph.clsNodeCache .= updatedCache
    pure (uuid, graph)

runInternalBreadcrumb :: Breadcrumb BreadcrumbItem -> Command Graph.Graph a
    -> Command Graph.Graph a
runInternalBreadcrumb breadcrumb act = do
    graph <- get
    let breadcrumbHierarchy
            = graph ^. Graph.userState . Graph.breadcrumbHierarchy
    case breadcrumbHierarchy `navigateTo` breadcrumb of
        Just h -> do
            env <- ask
            let newGraph
                    = graph & Graph.userState . Graph.breadcrumbHierarchy .~ h
            (res, state) <- liftIO $ runEmpire env newGraph act
            let modified = replaceAt breadcrumb breadcrumbHierarchy
                    $ state ^. Graph.userState . Graph.breadcrumbHierarchy
            newBc <- maybe
                (throwM $ BH.BreadcrumbDoesNotExistException breadcrumb)
                return
                modified
            let newGraph' = state & Graph.userState . Graph.breadcrumbHierarchy
                    .~ newBc
            put newGraph'
            return res
        _ -> throwM $ BH.BreadcrumbDoesNotExistException breadcrumb


withRootedFunction :: NodeId -> Command Graph.Graph a
    -> Command Graph.ClsGraph a
withRootedFunction uuid act = do
    graph <- preuse
        (Graph.userState . Graph.clsFuns . ix uuid . Graph.funGraph)
        <?!> BH.BreadcrumbDoesNotExistException (Breadcrumb [Definition uuid])
    env      <- ask
    state    <- get
    clsGraph <- use Graph.userState
    functionMarkers
        <- use $ Graph.userState . Graph.clsFuns . ix uuid . Graph.funMarkers
    let properGraph = let clsCode        = clsGraph ^. Graph.code
                          clsCodeMarkers = clsGraph ^. Graph.clsCodeMarkers
                          clsParseError  = clsGraph ^. Graph.clsParseError
                      in graph & Graph.code .~ clsCode
                               & Graph.codeMarkers .~ functionMarkers
                               & Graph.globalMarkers .~ clsCodeMarkers
                               & Graph.parseError .~ clsParseError
        properState = state & Graph.userState .~ properGraph

    ((res, len), newGraph) <- liftIO $ runEmpire env properState $ do
        a   <- act
        len <- runASTOp $ getLayer @SpanLength =<< ASTRead.getCurrentASTRef
        return (a, len)
    Graph.userState . Graph.clsFuns . ix uuid . Graph.funGraph
        .= newGraph ^. Graph.userState

    let lookupSpan f = view CodeSpan.realSpan <$> getLayer @CodeSpan.CodeSpan f
        updateLayer fun off spanLen = putLayer @CodeSpan.CodeSpan fun
            $ CodeSpan.mkRealSpan (LeftSpacedSpan (SpacedSpan off spanLen))

    diffs <- runASTOp $ do
        cls <- use Graph.clsClass
        funs <- ASTRead.classFunctions cls
        forM funs $ \fun -> ASTRead.cutThroughDocAndMarked fun >>= \f ->
            matchExpr f $ \case
                ASGFunction _ _ _ -> do
                    nodeId <- ASTRead.getNodeId fun
                    if nodeId == Just uuid then Just <$>
                        if fun == f then do
                            LeftSpacedSpan (SpacedSpan off prevLen)
                                <- lookupSpan f
                            updateLayer fun off len
                            pure $ len - prevLen
                        else do
                            LeftSpacedSpan (SpacedSpan off funLen)
                                <- lookupSpan f
                            updateLayer f off len
                            let diff = len - funLen
                            LeftSpacedSpan (SpacedSpan off' markedLen)
                                <- lookupSpan fun
                            updateLayer fun off' $ markedLen + diff
                            pure diff
                    else pure Nothing

    let diff = fromMaybe
            (error "function not in AST?")
            . listToMaybe $ catMaybes diffs
        funOffset = properGraph ^. Graph.fileOffset
    Graph.userState . Graph.clsFuns . traverse . Graph.funGraph
        . Graph.fileOffset %= (\a -> if a > funOffset then a + diff else a)
    Graph.userState . Graph.clsFuns . ix uuid . Graph.funMarkers
        .= newGraph ^. Graph.userState . Graph.codeMarkers
    Graph.userState . Graph.clsCodeMarkers
        %= \m -> Map.union m (newGraph ^. Graph.userState . Graph.codeMarkers)
    Graph.userState . Graph.code
        .= newGraph ^. Graph.userState . Graph.code
    Graph.userState . Graph.clsParseError
        .= newGraph ^. Graph.userState . Graph.parseError
    pure res

zoomInternalBreadcrumb :: Breadcrumb BreadcrumbItem -> Command Graph.Graph a
    -> Command Graph.Graph a
zoomInternalBreadcrumb (Breadcrumb (Definition _ : rest))
    = zoomInternalBreadcrumb (Breadcrumb rest)
zoomInternalBreadcrumb breadcrumb = runInternalBreadcrumb breadcrumb

zoomBreadcrumb
    :: Breadcrumb BreadcrumbItem
    -> Command Graph.Graph a
    -> Command Graph.ClsGraph a
    -> Command Library.Library a
zoomBreadcrumb = zoomCommand Library.body .:. zoomBreadcrumb' where
    zoomBreadcrumb' (Breadcrumb [])                       _actG  actC = actC
    zoomBreadcrumb' (Breadcrumb (Definition uuid : rest))  actG _actC
        = withRootedFunction uuid $ runInternalBreadcrumb (Breadcrumb rest) actG
    zoomBreadcrumb' breadcrumb _ _
        = throwM $ BH.BreadcrumbDoesNotExistException breadcrumb


module NodeEditor.Action.State.NodeEditor where

import Common.Prelude hiding (get)
import Common.Report  (warning)

import qualified Control.Monad.State                         as M
import qualified Data.HashMap.Strict                         as HashMap
import qualified Data.Map.Lazy                               as Map
import qualified Data.Set                                    as Set
import qualified JS.Visualizers                              as JS
import qualified LunaStudio.Data.PortRef                     as PortRef
import qualified NodeEditor.Action.Batch                     as Batch
import qualified NodeEditor.Action.State.Internal.NodeEditor as Internal
import qualified NodeEditor.React.Model.Layout               as Scene
import qualified NodeEditor.React.Model.Node.ExpressionNode  as ExpressionNode
import qualified NodeEditor.React.Model.NodeEditor           as NE
import qualified NodeEditor.React.Model.Port                 as Port
import qualified NodeEditor.React.Model.Searcher             as Searcher
import qualified NodeEditor.React.Model.Searcher.Hint.Node   as SearcherHint
import qualified NodeEditor.React.Model.Searcher.Mode        as SearcherMode
import qualified NodeEditor.React.Model.Searcher.Mode.Node   as SearcherModeNode
import qualified NodeEditor.React.Model.Visualization        as Visualization
import qualified NodeEditor.State.Global                     as Global

import Common.Action.Command                      (Command)
import Control.Arrow                              ((&&&))
import Data.Map.Lazy                              (Map)
import Data.Monoid                                (First (First), getFirst)
import LunaStudio.Data.CameraTransformation       (CameraTransformation)
import LunaStudio.Data.GraphLocation              (breadcrumb)
import LunaStudio.Data.MonadPath                  (MonadPath)
import LunaStudio.Data.NodeMeta                   (NodeMeta)
import LunaStudio.Data.Port                       (_WithDefault)
import LunaStudio.Data.PortDefault                (PortDefault)
import LunaStudio.Data.PortRef                    (AnyPortRef (..),
                                                   InPortRef (..),
                                                   OutPortRef (..))
import LunaStudio.Data.Position                   (Position)
import LunaStudio.Data.TypeRep                    (TypeRep (TStar),
                                                   toConstructorRep)
import LunaStudio.Data.Visualizer                 (applyType,
                                                   fromJSInternalVisualizersMap,
                                                   fromJSVisualizersMap)
import NodeEditor.Action.State.App                (get, getWorkspace, modify,
                                                   modifyApp)
import NodeEditor.Action.UUID                     (getUUID)
import NodeEditor.Batch.Workspace                 (currentLocation)
import NodeEditor.Data.Graph                      (Graph (Graph))
import NodeEditor.React.Model.App                 (nodeEditor)
import NodeEditor.React.Model.Breadcrumbs         (isTopLevel)
import NodeEditor.React.Model.Connection          (Connection, ConnectionId,
                                                   ConnectionsMap,
                                                   HalfConnection,
                                                   PosConnection, connectionId,
                                                   containsNode,
                                                   containsPortRef, dstNodeLoc,
                                                   srcNodeLoc, toConnectionsMap)
import NodeEditor.React.Model.Layout              (Layout, Scene)
import NodeEditor.React.Model.Node                (InputNode, Node (Expression, Input, Output),
                                                   NodeLoc, OutputNode,
                                                   inPortAt, inPortsList,
                                                   nodeLoc, outPortAt,
                                                   outPortsList, toNodesMap)
import NodeEditor.React.Model.Node.ExpressionNode (ExpressionNode, isSelected)
import NodeEditor.React.Model.NodeEditor          (GraphStatus, NodeEditor)
import NodeEditor.React.Model.Port                (InPort, OutPort, state)
import NodeEditor.React.Model.Searcher            (Searcher)
import NodeEditor.React.Model.Visualization       (ExternalVisualizers,
                                                   NodeVisualizations,
                                                   VisualizationBackup,
                                                   VisualizationId,
                                                   Visualizer (Visualizer),
                                                   VisualizerId (VisualizerId),
                                                   VisualizerPath,
                                                   VisualizerProperties (VisualizerProperties),
                                                   VisualizerType (ImportedVisualizer, LunaVisualizer, ProjectVisualizer),
                                                   Visualizers (Visualizers),
                                                   errorVisId,
                                                   mapExternalVisualizersWithKeyM,
                                                   placeholderVisId,
                                                   visualizerId, visualizerType,
                                                   _InternalVisualizer)
import NodeEditor.State.Global                    (State, internalVisualizers,
                                                   preferedVisualizers,
                                                   visualizers)


getNodeEditor :: Command State NodeEditor
getNodeEditor = get nodeEditor

modifyNodeEditor :: M.State NodeEditor r -> Command State r
modifyNodeEditor = modify nodeEditor

inTopLevelBreadcrumb :: Command State Bool
inTopLevelBreadcrumb = inTopLevelBreadcrumb' <$> getWorkspace where
    inTopLevelBreadcrumb' Nothing = False
    inTopLevelBreadcrumb' (Just w)
        = isTopLevel $ w ^. currentLocation . breadcrumb

isGraphLoaded :: Command State Bool
isGraphLoaded = view NE.isGraphLoaded <$> getNodeEditor

whenGraphLoaded :: Command State () -> Command State ()
whenGraphLoaded = whenM isGraphLoaded

setGraphStatus :: GraphStatus -> Command State ()
setGraphStatus graphStatus = modifyNodeEditor $ NE.graphStatus .= graphStatus

resetGraph :: Command State ()
resetGraph = modifyNodeEditor $ M.put def

resetApp :: Command State ()
resetApp = modifyApp $ M.put def

separateSubgraph :: [NodeLoc] -> Command State Graph
separateSubgraph nodeLocs = do
    let idSet = Set.fromList nodeLocs
        inSet = flip Set.member idSet
        mkMap = HashMap.fromList . map (view nodeLoc &&& id)
    nodes'
        <- HashMap.filterWithKey (inSet .: const) . mkMap <$> getExpressionNodes
    conns' <- HashMap.filter (inSet . view dstNodeLoc) <$> getConnectionsMap
    return $ Graph (HashMap.map convert nodes') (HashMap.map convert conns')

addNode :: Node -> Command State ()
addNode (Expression node) = addExpressionNode node
addNode (Input      node) = addInputNode node
addNode (Output     node) = addOutputNode node

addExpressionNode :: ExpressionNode -> Command State ()
addExpressionNode node = Internal.addNodeRec
    NE.expressionNodes
    ExpressionNode.expressionNodes
    (node ^. nodeLoc)
    node

addInputNode :: InputNode -> Command State ()
addInputNode node = Internal.setNodeRec
    NE.inputNode
    ExpressionNode.inputNode
    (node ^. nodeLoc)
    node

addOutputNode :: OutputNode -> Command State ()
addOutputNode node = Internal.setNodeRec
    NE.outputNode
    ExpressionNode.outputNode
    (node ^. nodeLoc)
    node

findPredecessorPosition :: ExpressionNode -> Command State Position
findPredecessorPosition n
    = ExpressionNode.findPredecessorPosition n <$> getExpressionNodes

findSuccessorPosition :: ExpressionNode -> Command State Position
findSuccessorPosition n
    = ExpressionNode.findSuccessorPosition n <$> getExpressionNodes

getNode :: NodeLoc -> Command State (Maybe Node)
getNode nl = NE.getNode nl <$> getNodeEditor

getAllNodes :: Command State [Node]
getAllNodes = do
    inputNodes      <- getInputNodes
    outputNodes     <- getOutputNodes
    expressionNodes <- getExpressionNodes
    return
        $  map Input      inputNodes
        <> map Output     outputNodes
        <> map Expression expressionNodes

getInputNode :: NodeLoc -> Command State (Maybe InputNode)
getInputNode nl = NE.getInputNode nl <$> getNodeEditor

getOutputNode :: NodeLoc -> Command State (Maybe OutputNode)
getOutputNode nl = NE.getOutputNode nl <$> getNodeEditor

getInputNodes :: Command State [InputNode]
getInputNodes = view NE.inputNodesRecursive <$> getNodeEditor

getOutputNodes :: Command State [OutputNode]
getOutputNodes = view NE.outputNodesRecursive <$> getNodeEditor

getExpressionNode :: NodeLoc -> Command State (Maybe ExpressionNode)
getExpressionNode nl = NE.getExpressionNode nl <$> getNodeEditor

getExpressionNodes :: Command State [ExpressionNode]
getExpressionNodes = view NE.expressionNodesRecursive <$> getNodeEditor

modifyExpressionNode :: Monoid r
    => NodeLoc -> M.State ExpressionNode r -> Command State r
modifyExpressionNode
  = Internal.modifyNodeRec NE.expressionNodes ExpressionNode.expressionNodes

modifyExpressionNodes_ :: M.State ExpressionNode () -> Command State ()
modifyExpressionNodes_ = void . modifyExpressionNodes

modifyExpressionNodes :: M.State ExpressionNode r -> Command State [r]
modifyExpressionNodes modifier = do
    nodeLocs <- view ExpressionNode.nodeLoc `fmap2` getExpressionNodes --FIXME it can be done faster
    catMaybes . map getFirst <$> forM
        nodeLocs
        (flip modifyExpressionNode $ (fmap (First . Just) modifier))

modifyInputNode :: Monoid r
    => NodeLoc -> M.State InputNode r -> Command State r
modifyInputNode
    = Internal.modifySidebarRec NE.inputNode ExpressionNode.inputNode

modifyOutputNode :: Monoid r
    => NodeLoc -> M.State OutputNode r -> Command State r
modifyOutputNode
    = Internal.modifySidebarRec NE.outputNode ExpressionNode.outputNode

removeNode :: NodeLoc -> Command State ()
removeNode
    = Internal.removeNodeRec NE.expressionNodes ExpressionNode.expressionNodes

getSelectedNodes :: Command State [ExpressionNode]
getSelectedNodes = filter (view isSelected) <$> getExpressionNodes

updateInputNode :: Maybe InputNode -> Command State ()
updateInputNode update = modifyNodeEditor $ NE.inputNode .= update

updateOutputNode :: Maybe OutputNode -> Command State ()
updateOutputNode update = modifyNodeEditor $ NE.outputNode .= update

updateExpressionNodes :: [ExpressionNode] -> Command State ()
updateExpressionNodes update
    = modifyNodeEditor $ NE.expressionNodes .= toNodesMap update

addConnection :: Connection -> Command State ()
addConnection conn = modifyNodeEditor $ NE.connections . at connId ?= conn where
    connId = conn ^. connectionId

getConnection :: ConnectionId -> Command State (Maybe Connection)
getConnection connId = HashMap.lookup connId <$> getConnectionsMap

getConnections :: Command State [Connection]
getConnections = HashMap.elems <$> getConnectionsMap

getPosConnection :: ConnectionId -> Command State (Maybe PosConnection)
getPosConnection connId = do
    mayConnection <- getConnection connId
    ne <- getNodeEditor
    return $ join $ NE.toPosConnection ne <$> mayConnection

getPosConnections :: Command State [PosConnection]
getPosConnections = view NE.posConnections <$> getNodeEditor

getConnectionsMap :: Command State ConnectionsMap
getConnectionsMap = view NE.connections <$> getNodeEditor

getConnectionsBetweenNodes :: NodeLoc -> NodeLoc -> Command State [Connection]
getConnectionsBetweenNodes nl1 nl2 = filter
    (\conn -> containsNode nl1 conn && containsNode nl2 conn)
    <$> getConnections

getConnectionsContainingNode :: NodeLoc -> Command State [Connection]
getConnectionsContainingNode nl = filter (containsNode nl) <$> getConnections

getConnectionsContainingNodes :: [NodeLoc] -> Command State [Connection]
getConnectionsContainingNodes nodeLocs
    = filter containsNode' <$> getConnections where
        nodeLocsSet = Set.fromList nodeLocs
        containsNode' conn
            = Set.member (conn ^. srcNodeLoc) nodeLocsSet
            || Set.member (conn ^. dstNodeLoc) nodeLocsSet

getConnectionsContainingPortRef :: AnyPortRef -> Command State [Connection]
getConnectionsContainingPortRef portRef
    = filter (containsPortRef portRef) <$> getConnections

getConnectionsFromNode :: NodeLoc -> Command State [Connection]
getConnectionsFromNode nl
    = filter (\conn -> conn ^. srcNodeLoc == nl) <$> getConnections

getConnectionsToNode :: NodeLoc -> Command State [Connection]
getConnectionsToNode nl
    = filter (\conn -> conn ^. dstNodeLoc == nl) <$> getConnections

modifyConnection :: Monoid r
    => ConnectionId -> M.State Connection r -> Command State r
modifyConnection connId
    = modify (nodeEditor . NE.connections . at connId) . zoom traverse

removeConnection :: ConnectionId -> Command State ()
removeConnection connId
    = modifyNodeEditor $ NE.connections . at connId .= Nothing

updateConnections :: [Connection] -> Command State ()
updateConnections update
    = modifyNodeEditor $ NE.connections .= toConnectionsMap update


getMonads :: Command State [MonadPath]
getMonads = view NE.monads <$> getNodeEditor

updateMonads :: [MonadPath] -> Command State ()
updateMonads update = modifyNodeEditor $ NE.monads .= update

getNodeMeta :: NodeLoc -> Command State (Maybe NodeMeta)
getNodeMeta = fmap2 (view ExpressionNode.nodeMeta) . getExpressionNode

modifyHalfConnections :: M.State [HalfConnection] r -> Command State r
modifyHalfConnections = modify (nodeEditor . NE.halfConnections)

getSearcher :: Command State (Maybe Searcher)
getSearcher = view NE.searcher <$> getNodeEditor

modifySearcher :: Monoid r => M.State Searcher r -> Command State r
modifySearcher = modify (nodeEditor . NE.searcher) . zoom traverse

getLayout :: Command State Layout
getLayout = view NE.layout <$> getNodeEditor

getScene :: Command State (Maybe Scene)
getScene = view Scene.scene <$> getLayout

getScreenTransform :: Command State CameraTransformation
getScreenTransform = view Scene.screenTransform <$> getLayout

setScreenTransform :: CameraTransformation -> Command State ()
setScreenTransform camera
    = modifyNodeEditor $ NE.layout . Scene.screenTransform .= camera

getNodeSearcherData :: Command State SearcherHint.Database
getNodeSearcherData = use Global.searcherDatabase

class NodeEditorElementId a where
    inGraph :: a -> Command State Bool
instance NodeEditorElementId NodeLoc where
    inGraph = fmap isJust . getNode
instance NodeEditorElementId ConnectionId where
    inGraph = fmap isJust . getConnection

getPort :: NE.GetPort a b => a -> Command State (Maybe b)
getPort portRef = NE.getPort portRef <$> getNodeEditor

modifyInPort :: Monoid r => InPortRef -> M.State InPort r -> Command State r
modifyInPort portRef action = modifyExpressionNode (portRef ^. PortRef.nodeLoc)
    $ zoom (inPortAt $ portRef ^. PortRef.dstPortId) action

modifyInPortsForNode :: Monoid r
    => NodeLoc -> M.State InPort r -> Command State ()
modifyInPortsForNode nl action = withJustM (getExpressionNode nl)
     $ \n -> mapM_
        (flip modifyInPort action)
        (InPortRef nl . view Port.portId <$> inPortsList n)

modifyOutPort :: Monoid r => OutPortRef -> M.State OutPort r -> Command State r
modifyOutPort portRef action = modifyExpressionNode (portRef ^. PortRef.nodeLoc)
    $ zoom (outPortAt $ portRef ^. PortRef.srcPortId) action

modifyOutPortsForNode :: Monoid r
    => NodeLoc -> M.State OutPort r -> Command State ()
modifyOutPortsForNode nl action = withJustM (getExpressionNode nl)
    $ \n -> mapM_
        (flip modifyOutPort action)
        (OutPortRef nl . view Port.portId <$> outPortsList n)

getPortDefault :: InPortRef -> Command State (Maybe PortDefault)
getPortDefault portRef = maybe
    Nothing
    (\mayPort -> mayPort ^? state . _WithDefault)
    <$> (NE.getPort portRef <$> getNodeEditor)

getLocalFunctions :: Command State [Text]
getLocalFunctions = do
    functionsNames <- toList . Set.fromList
        . fmap (view Port.name) . concatMap outPortsList <$> getAllNodes
    let getLambdaArgsNames s = fromMaybe mempty
            $ s ^? Searcher.mode . SearcherMode._Node
                . SearcherModeNode.mode . SearcherModeNode._ExpressionMode
                . SearcherModeNode.arguments
    lambdaArgsNames <- maybe mempty getLambdaArgsNames <$> getSearcher
    pure $ functionsNames <> lambdaArgsNames

getVisualizationsBackupMap :: Command State (Map NodeLoc VisualizationBackup)
getVisualizationsBackupMap
    = view (NE.visualizationsBackup . Visualization.backupMap) <$> getNodeEditor

getVisualizationBackup :: NodeLoc -> Command State (Maybe VisualizationBackup)
getVisualizationBackup nl = Map.lookup nl <$> getVisualizationsBackupMap

removeBackupForNodes :: [NodeLoc] -> Command State ()
removeBackupForNodes nls = modifyNodeEditor
    $ NE.visualizationsBackup . Visualization.backupMap
        %= \backupMap -> foldl (flip Map.delete) backupMap nls

updateVisualizers :: ExternalVisualizers FilePath -> Command State ()
updateVisualizers externalVisPaths = do
    internalVisPath <- liftIO $ JS.getInternalVisualizersLibraryPath
    lunaVisPath     <- liftIO $ JS.getLunaVisualizersLibraryPath
    modifyNodeEditor $ NE.visualizersLibPaths
        .= Visualizers internalVisPath lunaVisPath externalVisPaths
    let toMatcherMap tpe = Map.mapKeys (VisualizerId tpe) . fromJSVisualizersMap
    internalVisMap <- liftIO
        $ fromJSInternalVisualizersMap <$> JS.mkInternalVisualizersMap
    lunaVisMap <- liftIO
        $ toMatcherMap LunaVisualizer <$> JS.mkLunaVisualizersMap
    externalVis <- liftIO $ mapExternalVisualizersWithKeyM
        (\fp -> toMatcherMap
            ProjectVisualizer
            <$> JS.mkProjectVisualizersMap fp)
        (\libName fp -> toMatcherMap
            (ImportedVisualizer libName)
            <$> JS.mkImportedVisualizersMap (convert libName) fp)
        externalVisPaths
    let externalVisMap = foldl Map.union mempty externalVis

    Global.visualizers .= Map.union lunaVisMap externalVisMap
    Global.internalVisualizers .= internalVisMap

updateNodeVisualizers :: NodeLoc -> Command State ()
updateNodeVisualizers nl
    = getExpressionNodeType nl
    >>= maybe (return def) getVisualizersForType
    >>= applyVisualizers where
        applyVisualizers mayVis = modifyNodeEditor
            $ NE.nodeVisualizations . ix nl %= updateNodeVis mayVis
        updateSelectedVisId vis vp = vp & Visualization.selectedVisualizerId
            .~ (view visualizerId . fst <$> vis)
        updateRunningVis vis rv = rv & Visualization.visualizerProperties
            %~ updateSelectedVisId vis
        updateIdleVis vis iv = iv & Visualization.idleVisualizerProperties
            %~ updateSelectedVisId vis
        updateNodeVis vis nv = nv
            & Visualization.visualizations     %~ (updateRunningVis   vis <$>)
            & Visualization.idleVisualizations %~ (updateIdleVis      vis <$>)
            & Visualization.visualizers        .~ maybe def snd vis

getNodeVisualizations :: NodeLoc -> Command State (Maybe NodeVisualizations)
getNodeVisualizations nl = (^? NE.nodeVisualizations . ix nl) <$> getNodeEditor

getVisualizersForType :: TypeRep
    -> Command State (Maybe (Visualizer, Map VisualizerId VisualizerPath))
getVisualizersForType tpe = do
    mayPrefVis   <- HashMap.lookup tpe <$> use preferedVisualizers
    visualizers' <- use visualizers >>= applyType tpe
    let mayFirstVisInMap
            = fmap (uncurry Visualizer) . listToMaybe $ Map.toList visualizers'
        fromPrefVis vis = maybe
            mayFirstVisInMap
            (Just . Visualizer (vis ^. visualizerId))
            $ Map.lookup (vis ^. visualizerId) visualizers'
        mayDefVis = maybe mayFirstVisInMap fromPrefVis mayPrefVis
    return $ if isNothing mayDefVis || Map.null visualizers'
        then Nothing
        else (, visualizers') <$> mayDefVis

addVisualizationForNode :: NodeLoc -> Command State ()
addVisualizationForNode nl = withJustM (getExpressionNode nl) $ \n -> do
    mayVisualizer <- if ExpressionNode.returnsError n then getErrorVisualizer
        else
            if ExpressionNode.hasData n
            && isJust (n ^. ExpressionNode.defaultVisualizer)
                then return $ n ^. ExpressionNode.defaultVisualizer
                else getPlaceholderVisualizer
    withJust mayVisualizer $ \visualizer' -> do
        let newVis = Visualization.IdleVisualization
                Visualization.Ready
                $ VisualizerProperties
                    visualizer'
                    (Just $ visualizer' ^. visualizerId)
            updateNodeVisualizations _ nodeVis
                = nodeVis & Visualization.idleVisualizations %~ (newVis :)
        modifyNodeEditor $ NE.nodeVisualizations %= Map.insertWith
            updateNodeVisualizations
            nl
            (Visualization.NodeVisualizations def [newVis] def)

updateDefaultVisualizer :: NodeLoc -> Maybe Visualizer -> Bool
    -> Command State ()
updateDefaultVisualizer nl vis sendAsRequest = withJustM (getExpressionNode nl)
    $ \n -> when (n ^. ExpressionNode.defaultVisualizer /= vis) $ do
        modifyExpressionNode nl $ ExpressionNode.defaultVisualizer .= vis
        withJustM (getNodeMeta nl) $ \nm -> if sendAsRequest
            then Batch.setNodesMeta        $ Map.fromList [(nl, nm)]
            else Batch.sendNodesMetaUpdate $ Map.fromList [(nl, nm)]

recoverVisualizations :: NodeLoc -> Command State [VisualizationId]
recoverVisualizations nl = getNodeVisualizations nl >>= \case
    Nothing -> return def
    Just nodeVis -> do
        let (ready, outdated) = partition
                ((== Visualization.Ready)
                    . view Visualization.visualizationStatus)
                $ nodeVis ^. Visualization.idleVisualizations
        running <- fmap Map.fromList . forM ready $ \vis -> do
            visId <- getUUID
            liftIO $ JS.registerVisualizerFrame visId
            return (visId, Visualization.RunningVisualization
                visId
                def
                $ vis ^. Visualization.idleVisualizerProperties)
        modifyNodeEditor $ do
            NE.nodeVisualizations . ix nl . Visualization.visualizations
                %= Map.union running
            NE.nodeVisualizations . ix nl . Visualization.idleVisualizations
                .= outdated
        maybe
            def
            (Map.keys . view Visualization.visualizations)
            <$> getNodeVisualizations nl

getPlaceholderVisualizer :: Command State (Maybe Visualizer)
getPlaceholderVisualizer
    = fmap (Visualizer placeholderVisId) . Map.lookup placeholderVisId
    <$> use internalVisualizers

getErrorVisualizer :: Command State (Maybe Visualizer)
getErrorVisualizer = fmap (Visualizer errorVisId) . Map.lookup errorVisId
    <$> use internalVisualizers

clearVisualizationsForNode :: NodeLoc -> Command State ()
clearVisualizationsForNode nl
    = modifyNodeEditor $ NE.nodeVisualizations . ix nl .= def

setPlaceholderVisualization :: NodeLoc -> Command State [VisualizationId]
setPlaceholderVisualization nl = getExpressionNode nl >>= \mayN -> do
    case mayN of
        Nothing -> clearVisualizationsForNode nl
        Just n  -> getPlaceholderVisualizer >>= \case
            Nothing -> do
                warning "Visualization unavailable. Cannot find visualizer."
                clearVisualizationsForNode nl
            Just placeholderVis -> do
                mayVis <- maybe
                    (return def)
                    getVisualizersForType
                    $ n ^. ExpressionNode.nodeType
                modifyNodeEditor $ NE.nodeVisualizations %= \visMap -> do
                    let prevVis = maybe
                            def
                            (^. Visualization.visualizations)
                            $ Map.lookup nl visMap
                        running = Map.filter
                            ((placeholderVis ==)
                                . view (Visualization.visualizerProperties
                                . Visualization.runningVisualizer)
                            )
                            prevVis
                        idle = if Map.null running
                            then [Visualization.IdleVisualization
                                Visualization.Ready
                                $ Visualization.VisualizerProperties
                                    placeholderVis
                                    def
                                ]
                            else []
                        visualizers' = maybe def snd mayVis
                    Map.insert
                        nl
                        (Visualization.NodeVisualizations
                            running
                            idle
                            visualizers'
                        )
                        visMap
    recoverVisualizations nl

setErrorVisualization :: NodeLoc -> Command State [VisualizationId]
setErrorVisualization nl = getExpressionNode nl >>= \mayN -> do
    if isNothing mayN
        then clearVisualizationsForNode nl
        else getErrorVisualizer >>= \case
            Nothing -> do
                warning "Detailed error info unavailable. Cannot find error visualizer."
                clearVisualizationsForNode nl
            Just errorVis
                -> modifyNodeEditor $ NE.nodeVisualizations %= \visMap -> do
                    let prevVis = maybe
                            def
                            (^. Visualization.visualizations)
                            $ Map.lookup nl visMap
                        running = Map.filter
                            ((errorVis ==)
                                . view (Visualization.visualizerProperties
                                . Visualization.runningVisualizer)
                            )
                            prevVis
                        idle = if Map.null running
                            then [Visualization.IdleVisualization
                                Visualization.Ready
                                $ Visualization.VisualizerProperties
                                    errorVis
                                    def
                                ]
                            else []
                    Map.insert
                        nl
                        (Visualization.NodeVisualizations running idle def)
                        visMap
    recoverVisualizations nl

updateVisualizationsForNode :: NodeLoc -> Command State [VisualizationId]
updateVisualizationsForNode nl = getExpressionNode nl >>= \case
    Nothing -> clearVisualizationsForNode nl >> return def
    Just n -> if ExpressionNode.returnsError n
        then setErrorVisualization nl
        else do
            mayVis <- maybe
                (return def)
                getVisualizersForType
                $ n ^. ExpressionNode.nodeType
            if not (ExpressionNode.hasData n) || isNothing mayVis
                then setPlaceholderVisualization nl
                else withJust mayVis $ \vis -> do
                    let visualizer
                            = case n ^. ExpressionNode.defaultVisualizer of
                                Nothing     -> fst vis
                                Just defVis -> maybe
                                    (fst vis)
                                    (\vp -> defVis
                                        & Visualization.visualizerRelPath .~ vp)
                                    $ Map.lookup
                                        (defVis ^. visualizerId)
                                        $ snd vis
                    modifyNodeEditor $ NE.nodeVisualizations %= \visMap -> do
                        let prevVis = maybe
                                def
                                (^. Visualization.visualizations)
                                $ Map.lookup nl visMap
                            running = Map.filter
                                (not . has (Visualization.visualizerProperties
                                    . Visualization.runningVisualizer
                                    . visualizerId . visualizerType
                                    . _InternalVisualizer)
                                )
                                prevVis
                            idle = if Map.null running
                                then [Visualization.IdleVisualization
                                    Visualization.Ready
                                    $ Visualization.VisualizerProperties
                                        visualizer
                                        . Just $ visualizer ^. visualizerId
                                    ]
                                else []
                        Map.insert
                            nl
                            (Visualization.NodeVisualizations
                                running
                                idle
                                $ snd vis
                            )
                            visMap
                    updateDefaultVisualizer nl (Just visualizer) False
                    recoverVisualizations nl

updatePreferedVisualizer :: TypeRep -> Visualizer -> Command State ()
updatePreferedVisualizer tpe vis = preferedVisualizers . at tpe ?= vis

getExpressionNodeType :: NodeLoc -> Command State (Maybe TypeRep)
getExpressionNodeType
    = fmap (maybe def (view ExpressionNode.nodeType)) . getExpressionNode

setNodeProfilingData :: NodeLoc -> Integer -> Command State ()
setNodeProfilingData nl t
    = modifyExpressionNode nl $ ExpressionNode.execTime ?= t

isNewData :: NodeLoc -> VisualizationBackup -> Command State Bool
isNewData nl vp = (Just vp /=) <$> getVisualizationBackup nl

setVisualizationData :: NodeLoc -> VisualizationBackup -> Bool
    -> Command State ()
setVisualizationData nl backup@(Visualization.ValueBackup val) overwrite
    = whenM ((overwrite ||) <$> isNewData nl backup) $ do
        modifyNodeEditor $ NE.visualizationsBackup
            . Visualization.backupMap . at nl ?= backup
        visIds <- updateVisualizationsForNode nl
        withJustM (maybe def toConstructorRep <$> getExpressionNodeType nl)
            $ \cRep -> liftIO . forM_ visIds
                $ \visId -> JS.sendVisualizationData visId cRep val
setVisualizationData nl backup@(Visualization.StreamBackup values) _overwrite@True
    = whenM (isNewData nl backup) $ do
        modifyNodeEditor $ NE.visualizationsBackup
            . Visualization.backupMap . at nl ?= backup
        visIds <- updateVisualizationsForNode nl
        withJustM (maybe def toConstructorRep <$> getExpressionNodeType nl)
            $ \cRep -> liftIO . forM_ visIds
                $ \visId -> JS.notifyStreamRestart visId cRep $ reverse values
setVisualizationData nl (Visualization.StreamBackup values) _overwrite@False = do
    modifyNodeEditor $ NE.visualizationsBackup
        . Visualization.backupMap . ix nl
        . Visualization._StreamBackup %= (values <>)
    visIds <- maybe def (Map.keys . view Visualization.visualizations)
        <$> getNodeVisualizations nl
    liftIO . forM_ visIds $ forM_ values . JS.sendStreamDatapoint
setVisualizationData nl backup@(Visualization.MessageBackup msg) overwrite
    = whenM ((overwrite ||) <$> isNewData nl backup) $ do
        modifyNodeEditor $ NE.visualizationsBackup
            . Visualization.backupMap . at nl ?= backup
        visIds <- setPlaceholderVisualization nl
        liftIO . forM_ visIds $ flip JS.sendInternalData msg
setVisualizationData nl backup@(Visualization.ErrorBackup msg) overwrite
    = whenM ((overwrite ||) <$> isNewData nl backup) $ do
        modifyNodeEditor $ NE.visualizationsBackup
            . Visualization.backupMap . at nl ?= backup
        visIds <- setErrorVisualization nl
        liftIO . forM_ visIds $ flip JS.sendInternalData msg

resetSuccessors :: NodeLoc -> Command State ()
resetSuccessors nl = do
    outConnections <- filter (\c -> c ^. srcNodeLoc == nl) <$> getConnections
    let successors = view dstNodeLoc <$> outConnections
    whenM (resetNode nl) $ do
        mapM_ resetSuccessors successors

resetNode :: NodeLoc -> Command State Bool
resetNode nl = do
    maySuccess <- modifyExpressionNode nl $ do
        let resetPort = Port.valueType .~ TStar
        oldInPorts  <- use ExpressionNode.inPorts
        oldOutPorts <- use ExpressionNode.inPorts
        ExpressionNode.inPorts  %= fmap resetPort
        ExpressionNode.outPorts %= fmap resetPort
        ExpressionNode.value .= def
        newInPorts  <- use ExpressionNode.inPorts
        newOutPorts <- use ExpressionNode.inPorts
        return . First . Just
            $ (oldInPorts /= newInPorts) && (oldOutPorts /= newOutPorts)
    setVisualizationData
        nl
        (Visualization.MessageBackup Visualization.awaitingDataMsg)
        True
    return $ fromMaybe False $ getFirst maySuccess

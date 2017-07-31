{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE TypeFamilies           #-}
module NodeEditor.Action.State.NodeEditor where

import           Common.Prelude                              hiding (get)
import           Control.Arrow                               ((&&&))
import qualified Control.Monad.State                         as M
import qualified Data.HashMap.Strict                         as HashMap
import           Data.Map.Lazy                               (Map)
import qualified Data.Map.Lazy                               as Map
import           Data.Monoid                                 (First (First), getFirst)
import qualified Data.Set                                    as Set
import           JS.Visualizers                              (registerVisualizerFrame)
import           LunaStudio.Data.CameraTransformation        (CameraTransformation)
import           LunaStudio.Data.MonadPath                   (MonadPath)
import qualified LunaStudio.Data.Node                        as Empire
import           LunaStudio.Data.NodeMeta                    (NodeMeta)
import qualified LunaStudio.Data.NodeSearcher                as NS
import           LunaStudio.Data.NodeValue                   (VisualizationId, Visualizer, VisualizerName, VisualizerPath, applyType)
import           LunaStudio.Data.Port                        (_WithDefault)
import           LunaStudio.Data.PortDefault                 (PortDefault)
import           LunaStudio.Data.PortRef                     (AnyPortRef (..), InPortRef (..), OutPortRef (..))
import qualified LunaStudio.Data.PortRef                     as PortRef
import           LunaStudio.Data.Position                    (Position)
import           LunaStudio.Data.TypeRep                     (TypeRep)
import qualified NodeEditor.Action.Batch                     as Batch
import           NodeEditor.Action.Command                   (Command)
import           NodeEditor.Action.State.App                 (get, modify, modifyApp)
import qualified NodeEditor.Action.State.Internal.NodeEditor as Internal
import           NodeEditor.Action.UUID                      (getUUID)
import           NodeEditor.Batch.Workspace                  (nodeSearcherData)
import           NodeEditor.Data.Graph                       (Graph (Graph))
import           NodeEditor.React.Model.App                  (nodeEditor)
import           NodeEditor.React.Model.Connection           (Connection, ConnectionId, ConnectionsMap, HalfConnection, PosConnection,
                                                              connectionId, containsNode, containsPortRef, dstNodeLoc, srcNodeLoc,
                                                              toConnectionsMap)
import           NodeEditor.React.Model.Layout               (Layout, Scene)
import qualified NodeEditor.React.Model.Layout               as Scene
import           NodeEditor.React.Model.Node                 (InputNode, Node (Expression, Input, Output), NodeLoc, OutputNode, inPortAt,
                                                              inPortsList, nodeLoc, outPortAt, outPortsList, toNodesMap)
import           NodeEditor.React.Model.Node.ExpressionNode  (ExpressionNode, isSelected)
import qualified NodeEditor.React.Model.Node.ExpressionNode  as ExpressionNode
import           NodeEditor.React.Model.NodeEditor           (GraphStatus, NodeEditor, VisualizationBackup)
import qualified NodeEditor.React.Model.NodeEditor           as NE
import           NodeEditor.React.Model.Port                 (InPort, OutPort, state)
import qualified NodeEditor.React.Model.Port                 as Port
import           NodeEditor.React.Model.Searcher             (Searcher)
import qualified NodeEditor.React.Model.Searcher             as Searcher
import           NodeEditor.React.Model.Visualization        (NodeVisualizations)
import qualified NodeEditor.React.Model.Visualization        as Visualization
import           NodeEditor.State.Global                     (State, preferedVisualizers, visualizers, workspace)
import           Text.ScopeSearcher.Item                     (Items, isElement)


getNodeEditor :: Command State NodeEditor
getNodeEditor = get nodeEditor

modifyNodeEditor :: M.State NodeEditor r -> Command State r
modifyNodeEditor = modify nodeEditor

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
    nodes' <- HashMap.filterWithKey (inSet .: const) . mkMap <$> getExpressionNodes
    conns' <- HashMap.filter (inSet . view dstNodeLoc) <$> getConnectionsMap
    return $ Graph (HashMap.map convert nodes') (HashMap.map convert conns')

addNode :: Node -> Command State ()
addNode (Expression node) = addExpressionNode node
addNode (Input      node) = addInputNode node
addNode (Output     node) = addOutputNode node

addExpressionNode :: ExpressionNode -> Command State ()
addExpressionNode node = Internal.addNodeRec NE.expressionNodes ExpressionNode.expressionNodes (node ^. nodeLoc) node

addInputNode :: InputNode -> Command State ()
addInputNode node = Internal.setNodeRec NE.inputNode ExpressionNode.inputNode (node ^. nodeLoc) node

addOutputNode :: OutputNode -> Command State ()
addOutputNode node = Internal.setNodeRec NE.outputNode ExpressionNode.outputNode (node ^. nodeLoc) node

findPredecessorPosition :: ExpressionNode -> Command State Position
findPredecessorPosition n = ExpressionNode.findPredecessorPosition n <$> getExpressionNodes

findSuccessorPosition :: ExpressionNode -> Command State Position
findSuccessorPosition n = ExpressionNode.findSuccessorPosition n <$> getExpressionNodes

getNode :: NodeLoc -> Command State (Maybe Node)
getNode nl = NE.getNode nl <$> getNodeEditor

getAllNodes :: Command State [Node]
getAllNodes = do
    inputNodes      <- getInputNodes
    outputNodes     <- getOutputNodes
    expressionNodes <- getExpressionNodes
    return $ map Input inputNodes <> map Output outputNodes <> map Expression expressionNodes

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

modifyExpressionNode :: Monoid r => NodeLoc -> M.State ExpressionNode r -> Command State r
modifyExpressionNode = Internal.modifyNodeRec NE.expressionNodes ExpressionNode.expressionNodes

modifyExpressionNodes_ :: M.State ExpressionNode () -> Command State ()
modifyExpressionNodes_ = void . modifyExpressionNodes

modifyExpressionNodes :: M.State ExpressionNode r -> Command State [r]
modifyExpressionNodes modifier = do
    nodeLocs <- view ExpressionNode.nodeLoc `fmap2` getExpressionNodes --FIXME it can be done faster
    catMaybes . map getFirst <$> forM nodeLocs (flip modifyExpressionNode $ (fmap (First . Just) modifier))

modifyInputNode :: Monoid r => NodeLoc -> M.State InputNode r -> Command State r
modifyInputNode = Internal.modifySidebarRec NE.inputNode ExpressionNode.inputNode

modifyOutputNode :: Monoid r => NodeLoc -> M.State OutputNode r -> Command State r
modifyOutputNode = Internal.modifySidebarRec NE.outputNode ExpressionNode.outputNode

removeNode :: NodeLoc -> Command State ()
removeNode = Internal.removeNodeRec NE.expressionNodes ExpressionNode.expressionNodes

getSelectedNodes :: Command State [ExpressionNode]
getSelectedNodes = filter (view isSelected) <$> getExpressionNodes

updateInputNode :: Maybe InputNode -> Command State ()
updateInputNode update = modifyNodeEditor $ NE.inputNode .= update

updateOutputNode :: Maybe OutputNode -> Command State ()
updateOutputNode update = modifyNodeEditor $ NE.outputNode .= update

updateExpressionNodes :: [ExpressionNode] -> Command State ()
updateExpressionNodes update = modifyNodeEditor $ NE.expressionNodes .= toNodesMap update

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
getConnectionsBetweenNodes nl1 nl2 =
    filter (\conn -> containsNode nl1 conn && containsNode nl2 conn) <$> getConnections

getConnectionsContainingNode :: NodeLoc -> Command State [Connection]
getConnectionsContainingNode nl = filter (containsNode nl) <$> getConnections

getConnectionsContainingNodes :: [NodeLoc] -> Command State [Connection]
getConnectionsContainingNodes nodeLocs = filter containsNode' <$> getConnections where
    nodeLocsSet        = Set.fromList nodeLocs
    containsNode' conn = Set.member (conn ^. srcNodeLoc) nodeLocsSet
                      || Set.member (conn ^. dstNodeLoc) nodeLocsSet

getConnectionsContainingPortRef :: AnyPortRef -> Command State [Connection]
getConnectionsContainingPortRef portRef = filter (containsPortRef portRef) <$> getConnections

getConnectionsFromNode :: NodeLoc -> Command State [Connection]
getConnectionsFromNode nl = filter (\conn -> conn ^. srcNodeLoc == nl) <$> getConnections

getConnectionsToNode :: NodeLoc -> Command State [Connection]
getConnectionsToNode nl = filter (\conn -> conn ^. dstNodeLoc == nl) <$> getConnections

modifyConnection :: Monoid r => ConnectionId -> M.State Connection r -> Command State r
modifyConnection connId = modify (nodeEditor . NE.connections . at connId) . zoom traverse

removeConnection :: ConnectionId -> Command State ()
removeConnection connId = modifyNodeEditor $ NE.connections . at connId .= Nothing

updateConnections :: [Connection] -> Command State ()
updateConnections update = modifyNodeEditor $ NE.connections .= toConnectionsMap update


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

getScreenTranform :: Command State CameraTransformation
getScreenTranform = view Scene.screenTransform <$> getLayout

setScreenTransform :: CameraTransformation -> Command State ()
setScreenTransform camera = modifyNodeEditor $ NE.layout . Scene.screenTransform .= camera

globalFunctions :: Items a -> Items a
globalFunctions = Map.filter isElement

getNodeSearcherData :: Command State (Items Empire.ExpressionNode)
getNodeSearcherData = maybe def id <$> preuse (workspace . traverse . nodeSearcherData)

class NodeEditorElementId a where
    inGraph :: a -> Command State Bool
instance NodeEditorElementId NodeLoc where
    inGraph = fmap isJust . getNode
instance NodeEditorElementId ConnectionId where
    inGraph = fmap isJust . getConnection

getPort :: NE.GetPort a b => a -> Command State (Maybe b)
getPort portRef = NE.getPort portRef <$> getNodeEditor

modifyInPort :: Monoid r => InPortRef -> M.State InPort r -> Command State r
modifyInPort portRef action = modifyExpressionNode (portRef ^. PortRef.nodeLoc) $ zoom (inPortAt $ portRef ^. PortRef.dstPortId) action

modifyInPortsForNode :: Monoid r => NodeLoc -> M.State InPort r -> Command State ()
modifyInPortsForNode nl action = withJustM (getExpressionNode nl) $ \n -> do
    mapM_ (flip modifyInPort action) (map (InPortRef nl . view Port.portId) $ inPortsList n)

modifyOutPort :: Monoid r => OutPortRef -> M.State OutPort r -> Command State r
modifyOutPort portRef action = modifyExpressionNode (portRef ^. PortRef.nodeLoc) $ zoom (outPortAt $ portRef ^. PortRef.srcPortId) action

modifyOutPortsForNode :: Monoid r => NodeLoc -> M.State OutPort r -> Command State ()
modifyOutPortsForNode nl action = withJustM (getExpressionNode nl) $ \n -> do
    mapM_ (flip modifyOutPort action) (map (OutPortRef nl . view Port.portId) $ outPortsList n)

getPortDefault :: InPortRef -> Command State (Maybe PortDefault)
getPortDefault portRef = maybe Nothing (\mayPort -> mayPort ^? state . _WithDefault) <$> (NE.getPort portRef <$> getNodeEditor)

getLocalFunctions :: Command State (Items Empire.ExpressionNode)
getLocalFunctions = do
    functionsNames  <- Set.toList . Set.fromList . map (view Port.name) . concatMap outPortsList <$> getAllNodes
    searcherMode    <- fmap2 (view Searcher.mode) $ getSearcher
    let lambdaArgsNames = case searcherMode of
            Just (Searcher.Node _ (Searcher.NodeModeInfo _ _ argNames) _) -> argNames
            _                                                             -> []
    return . Map.fromList . map NS.entry $ (functionsNames <> lambdaArgsNames)

getVisualizationsBackupMap :: Command State (Map NodeLoc VisualizationBackup)
getVisualizationsBackupMap = view (NE.visualizationsBackup . NE.backupMap) <$> getNodeEditor

removeBackupForNodes :: [NodeLoc] -> Command State ()
removeBackupForNodes nls = modifyNodeEditor $ NE.visualizationsBackup . NE.backupMap %= \backupMap -> foldl (flip Map.delete) backupMap nls

getNodeVisualizations :: NodeLoc -> Command State (Maybe NodeVisualizations)
getNodeVisualizations nl = (^? NE.nodeVisualizations . ix nl) <$> getNodeEditor

getVisualizers :: TypeRep -> Command State (Maybe (Visualizer, Map VisualizerName VisualizerPath))
getVisualizers tpe = do
    mayPrefVis  <- HashMap.lookup tpe <$> use preferedVisualizers
    visualizers' <- use visualizers >>= applyType tpe
    let mayDefVis = case mayPrefVis of
            Just (prefName, prefPath) -> if Map.lookup prefName visualizers' == Just prefPath
                then mayPrefVis
                else listToMaybe $ Map.toList visualizers'
            _ -> listToMaybe $ Map.toList visualizers'
    return $ if isNothing mayDefVis || Map.null visualizers'
        then Nothing
        else (, visualizers') <$> mayDefVis

addVisualizationForNode :: NodeLoc -> Command State ()
addVisualizationForNode nl = withJustM (maybe def (view ExpressionNode.defaultVisualizer) <$> getExpressionNode nl) $ \visualizer' -> do
    let newVis = Visualization.IdleVisualization Visualization.Ready visualizer'
        updateNodeVisualizations _ nodeVis = nodeVis & Visualization.idleVisualizations %~ (newVis :)
    modifyNodeEditor $ NE.nodeVisualizations %= Map.insertWith updateNodeVisualizations
                                                               nl
                                                               (Visualization.NodeVisualizations def [newVis] def)

updateDefaultVisualizer :: NodeLoc -> Maybe Visualizer -> Bool -> Command State ()
updateDefaultVisualizer nl vis sendAsRequest = do
    modifyExpressionNode nl $ ExpressionNode.defaultVisualizer .= vis
    withJustM (getNodeMeta nl) $ \nm -> if sendAsRequest
        then Batch.setNodesMeta [(nl, nm)]
        else Batch.sendNodesMetaUpdate [(nl, nm)]

recoverVisualizations :: NodeLoc -> Command State [VisualizationId]
recoverVisualizations nl = getNodeVisualizations nl >>= \case
    Nothing      -> return def
    Just nodeVis -> do
        let (ready, outdated) = partition ((== Visualization.Ready) . view Visualization.visualizationStatus) $ nodeVis ^. Visualization.idleVisualizations
        running <- fmap Map.fromList . forM ready $ \vis -> do
            visId <- getUUID
            liftIO $ registerVisualizerFrame visId
            return (visId, Visualization.RunningVisualization visId def $ vis ^. Visualization.idleVisualizer)
        modifyNodeEditor $ do
            NE.nodeVisualizations . ix nl . Visualization.visualizations     %= Map.union running
            NE.nodeVisualizations . ix nl . Visualization.idleVisualizations .= outdated
        maybe def (Map.keys . view Visualization.visualizations) <$> getNodeVisualizations nl

updateVisualizationsForNode :: NodeLoc -> Maybe TypeRep -> Command State ()
updateVisualizationsForNode nl mayTpe = do
    mayVisInfo <- maybe (return def) getVisualizers mayTpe
    updateDefaultVisualizer nl (fst <$> mayVisInfo) False
    case mayVisInfo of
        Nothing -> modifyNodeEditor $ withJustM (preuse $ NE.nodeVisualizations . ix nl) $ \nodeVis -> do
            let idleVis = map (& Visualization.visualizationStatus .~ Visualization.Outdated) (nodeVis ^. Visualization.idleVisualizations)
                       ++ map (Visualization.toIdleVisualization Visualization.Outdated) (Map.elems $ nodeVis ^. Visualization.visualizations)
            NE.nodeVisualizations . at nl ?= Visualization.NodeVisualizations def idleVis def
        Just (defVisualizer, visualizers') -> do
            whenM (maybe True (\vis -> Map.null (vis ^. Visualization.visualizations) && null (vis ^. Visualization.idleVisualizations) ). (^? NE.nodeVisualizations . ix nl) <$> getNodeEditor) $
                addVisualizationForNode nl
            modifyNodeEditor $ withJustM (preuse $ NE.nodeVisualizations . ix nl) $ \nodeVis -> do
                let updateRunningFoldFunction (running', ready') vis = if Map.lookup (vis ^. Visualization.runningVisualizer . _1) visualizers' == Just (vis ^. Visualization.runningVisualizer . _2)
                        then (Map.insert (vis ^. Visualization.visualizationId) vis running', ready')
                        else (running', (Visualization.toIdleVisualization Visualization.Ready vis) : ready')
                    (running, ready'') = foldl updateRunningFoldFunction (def, def) . Map.elems $ nodeVis ^. Visualization.visualizations
                    makeVisReady vis = if Map.lookup (vis ^. Visualization.idleVisualizer . _1) visualizers' == Just (vis ^. Visualization.idleVisualizer . _2)
                        then vis & Visualization.visualizationStatus .~ Visualization.Ready
                        else Visualization.IdleVisualization Visualization.Ready defVisualizer
                    ready            = ready'' ++ map makeVisReady (nodeVis ^. Visualization.idleVisualizations)
                NE.nodeVisualizations . at nl ?= Visualization.NodeVisualizations running ready visualizers'

updatePreferedVisualizer :: TypeRep -> Visualizer -> Command State ()
updatePreferedVisualizer tpe vis = preferedVisualizers . at tpe ?= vis

getExpressionNodeType :: NodeLoc -> Command State (Maybe TypeRep)
getExpressionNodeType = fmap (maybe def (view ExpressionNode.nodeType)) . getExpressionNode

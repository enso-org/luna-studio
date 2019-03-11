module NodeEditor.React.Model.NodeEditor where

import Common.Prelude

import qualified Data.HashMap.Strict                        as HashMap
import qualified Data.Map                                   as Map
import qualified LunaStudio.Data.Breadcrumb                 as B
import qualified LunaStudio.Data.Error                      as Error
import qualified LunaStudio.Data.NodeLoc                    as NodeLoc
import qualified LunaStudio.Data.PortRef                    as PortRef
import qualified NodeEditor.React.Model.Connection          as Connection
import qualified NodeEditor.React.Model.Layout              as Layout
import qualified NodeEditor.React.Model.Node.ExpressionNode as ExpressionNode
import qualified NodeEditor.React.Model.Port                as Port
import qualified NodeEditor.React.Model.Searcher            as Searcher
import qualified NodeEditor.React.Model.Visualization       as Visualization


import Data.Map                             (Map)
import LunaStudio.Data.CameraTransformation (CameraTransformation)
import LunaStudio.Data.MonadPath            (MonadPath)
import NodeEditor.Data.Color                (Color (Color))
import NodeEditor.React.Model.Connection    (Connection, ConnectionsMap,
                                             HalfConnection (HalfConnection),
                                             PosConnection (PosConnection),
                                             PosHalfConnection (PosHalfConnection))
import NodeEditor.React.Model.ConnectionPen (ConnectionPen)
import NodeEditor.React.Model.Layout        (Layout)
import NodeEditor.React.Model.Node          (ExpressionNode, ExpressionNodesMap,
                                             HasNodeLoc, InputNode,
                                             Node (Expression, Input, Output),
                                             NodeLoc, OutputNode, countArgPorts,
                                             hasPort, lookupPort, nodeId)
import NodeEditor.React.Model.Port          (AnyPort,
                                             AnyPortId (InPortId', OutPortId'),
                                             AnyPortRef (OutPortRef'), InPort,
                                             InPortRef, OutPort, OutPortRef,
                                             getPortNumber)
import NodeEditor.React.Model.Searcher      (Searcher)
import NodeEditor.React.Model.SelectionBox  (SelectionBox)
import NodeEditor.React.Model.Visualization (NodeVisualizations, VisualizationProperties (VisualizationProperties),
                                             VisualizationsBackupMap,
                                             Visualizers)


data GraphStatus
    = GraphLoaded
    | GraphLoading
    | NoGraph
    | GraphError (Error.Error Error.GraphError)
    deriving (Eq, Generic)

makePrisms ''GraphStatus

data NodeEditor = NodeEditor
    { _expressionNodes          :: ExpressionNodesMap
    , _inputNode                :: Maybe InputNode
    , _outputNode               :: Maybe OutputNode
    , _monads                   :: [MonadPath]
    , _connections              :: ConnectionsMap
    , _visualizersLibPaths      :: Visualizers FilePath
    , _nodeVisualizations       :: Map NodeLoc NodeVisualizations
    , _visualizationsBackup     :: VisualizationsBackupMap

    , _halfConnections          :: [HalfConnection]
    , _connectionPen            :: Maybe ConnectionPen
    , _selectionBox             :: Maybe SelectionBox
    , _searcher                 :: Maybe Searcher
    , _textControlEditedPortRef :: Maybe InPortRef

    , _graphStatus              :: GraphStatus
    , _layout                   :: Layout
    , _topZIndex                :: Int
    } deriving (Eq, Generic)

instance Default NodeEditor where
    def = NodeEditor
        {- expressionNodes          -} def
        {- inputNode                -} def
        {- outputNode               -} def
        {- monads                   -} def
        {- connections              -} def
        {- visualizersLibPath       -} def
        {- visualizations           -} def
        {- visualizationsBackup     -} def
        {- halfConnections          -} def
        {- connectionPen            -} def
        {- selectionBox             -} def
        {- searcher                 -} def
        {- textControlEditedPortRef -} def
        {- graphStatus              -} NoGraph
        {- layout                   -} def
        {- topZIndex                -} def

makeLenses ''NodeEditor

isGraphLoaded :: Getter NodeEditor Bool
isGraphLoaded = graphStatus . to (== GraphLoaded)

returnsGraphError :: Getter NodeEditor Bool
returnsGraphError = to (has (graphStatus . _GraphError))

screenTransform :: Lens' NodeEditor CameraTransformation
screenTransform = layout . Layout.screenTransform

searcherProperties :: Getter NodeEditor (Maybe Searcher.Properties)
searcherProperties = to $ \ne -> let
    lunaVisPath = ne ^. visualizersLibPaths . Visualization.lunaVisualizers
    maySearcher = ne ^. searcher
    toProps     = \s -> Searcher.mkProperties s lunaVisPath
    in toProps <$> maySearcher

expressionNodesRecursive :: Getter NodeEditor [ExpressionNode]
expressionNodesRecursive = to (concatMap expressionNodesRecursive' . HashMap.elems . view expressionNodes) where
    expressionNodesRecursive' node = node : concatMap expressionNodesRecursive' (concatMap (HashMap.elems . view ExpressionNode.expressionNodes) (node ^. ExpressionNode.subgraphs))

inputNodesRecursive :: Getter NodeEditor [InputNode]
inputNodesRecursive = to inputNodesRecursive' where
    inputNodesRecursive' ne = maybeToList (ne ^. inputNode) <> concatMap inputNodesRec (HashMap.elems $ ne ^. expressionNodes)
    inputNodesRec node = (concatMap (maybeToList . view ExpressionNode.inputNode) (node ^. ExpressionNode.subgraphs))
                       <> concatMap inputNodesRec (concatMap (HashMap.elems . view ExpressionNode.expressionNodes) (node ^. ExpressionNode.subgraphs))

outputNodesRecursive :: Getter NodeEditor [OutputNode]
outputNodesRecursive = to outputNodesRecursive' where
    outputNodesRecursive' ne = maybeToList (ne ^. outputNode) <> concatMap outputNodesRec (HashMap.elems $ ne ^. expressionNodes)
    outputNodesRec node = (concatMap (maybeToList . view ExpressionNode.outputNode) (node ^. ExpressionNode.subgraphs))
                        <> concatMap outputNodesRec (concatMap (HashMap.elems . view ExpressionNode.expressionNodes) (node ^. ExpressionNode.subgraphs))

getExpressionNode :: NodeLoc -> NodeEditor -> Maybe ExpressionNode
getExpressionNode nl ne = getNodeRec' (nl ^. NodeLoc.pathItems) where
    getNodeRec' []    = ne ^. expressionNodes . at (nl ^. NodeLoc.nodeId)
    getNodeRec' (h:t) = getSubNode t =<< getSubgraph h =<< getRootNode (h ^. B.nodeId)

    getSubNode []    = preview (ExpressionNode.expressionNodes . at (nl ^. NodeLoc.nodeId) . traverse)
    getSubNode (h:t) = getSubNode t <=< getSubgraph h <=< getExprNode (h ^. B.nodeId)

    getSubgraph item = preview (ExpressionNode.subgraphs . at item . traverse)
    getExprNode nid  = preview (ExpressionNode.expressionNodes . at nid . traverse)

    getRootNode nid = ne ^. expressionNodes . at nid

updateExpressionNode :: ExpressionNode -> NodeEditor -> NodeEditor
updateExpressionNode n ne = updateExpressionNode' (nl ^. NodeLoc.pathItems) where
    nl                          = n ^. ExpressionNode.nodeLoc
    updateExpressionNode' []    = ne & expressionNodes . at (nl ^. nodeId)  ?~ n
    updateExpressionNode' (h:t) = ne & expressionNodes . ix (h ^. B.nodeId) %~ updateNode t

    updateNode []    _  = n
    updateNode (h:t) n' = n' & ExpressionNode.subgraphs . ix h . ExpressionNode.expressionNodes . ix (h ^. B.nodeId) %~ updateNode t


getInputNode :: NodeLoc -> NodeEditor -> Maybe InputNode
getInputNode = _getNodeRec inputNode ExpressionNode.inputNode

getOutputNode :: NodeLoc -> NodeEditor -> Maybe OutputNode
getOutputNode = _getNodeRec outputNode ExpressionNode.outputNode

getNode :: NodeLoc -> NodeEditor -> Maybe Node
getNode nl ne = maybe (maybe (Input <$> getInputNode nl ne) (Just . Output) $ getOutputNode nl ne) (Just . Expression) $ getExpressionNode nl ne

_getNodeRec :: HasNodeLoc a => Lens' NodeEditor (Maybe a) -> Lens' ExpressionNode.Subgraph (Maybe a) -> NodeLoc -> NodeEditor -> Maybe a
_getNodeRec rootLens subLens nl ne = getNodeRec' (nl ^. NodeLoc.pathItems) where
    getNodeRec' []    = ne ^? rootLens . filtered ((== Just (nl ^. nodeId)) . fmap (view nodeId)) . traverse
    getNodeRec' (h:t) = getSubNode t =<< getSubgraph h =<< getRootNode (h ^. B.nodeId)

    getSubNode []    = preview (subLens . filtered ((== Just (nl ^. nodeId)) . fmap (view nodeId)) . traverse)
    getSubNode (h:t) = getSubNode t <=< getSubgraph h <=< getExprNode (h ^. B.nodeId)

    getSubgraph item = preview (ExpressionNode.subgraphs . at item . traverse)
    getExprNode nid  = preview (ExpressionNode.expressionNodes . at nid . traverse)

    getRootNode nid = ne ^. expressionNodes . at nid

posConnections :: Getter NodeEditor [PosConnection]
posConnections = to getConnections where
    getConnections ne = mapMaybe (toPosConnection ne) (ne ^. connections . to HashMap.elems)

posHalfConnections :: Getter NodeEditor [PosHalfConnection]
posHalfConnections = to getConnections where
    getConnections ne = mapMaybe (toPosHalfConnection ne) (ne ^. halfConnections)

class GetPort a b | a -> b where
    getPort :: a -> NodeEditor -> Maybe b

instance GetPort InPortRef InPort where
    getPort portRef ne = getNode (portRef ^. PortRef.nodeLoc) ne >>= flip lookupPort (portRef ^. PortRef.dstPortId)
instance GetPort OutPortRef OutPort where
    getPort portRef ne = getNode (portRef ^. PortRef.nodeLoc) ne >>= flip lookupPort (portRef ^. PortRef.srcPortId)
instance GetPort AnyPortRef AnyPort where
    getPort portRef ne = getNode (portRef ^. PortRef.nodeLoc) ne >>= flip lookupPort (portRef ^. PortRef.portId)

atMostFirstLevel :: [a] -> [a]
atMostFirstLevel []    = []
atMostFirstLevel (h:_) = [h]


atMostFirstLevel' :: AnyPortId -> AnyPortId
atMostFirstLevel' (InPortId'  p) = InPortId'  $ atMostFirstLevel p
atMostFirstLevel' (OutPortId' p) = OutPortId' $ atMostFirstLevel p


toPosConnection :: NodeEditor -> Connection -> Maybe PosConnection
toPosConnection ne connection = do
    let src         = connection ^. Connection.src
        dst         = connection ^. Connection.dst
        dstPortRef  = dst & PortRef.dstPortId %~ atMostFirstLevel
        srcNodeLoc  = src ^. PortRef.srcNodeLoc
        dstNodeLoc  = dstPortRef ^. PortRef.dstNodeLoc
        dstPortId   = dstPortRef ^. PortRef.dstPortId
        mode        = connection ^. Connection.mode
        sidebarConn = connection ^. Connection.sidebarConn
    srcNode <- getNode srcNodeLoc ne
    dstNode <- getNode dstNodeLoc ne
    srcPort <- getPort src ne
    if hasPort dstPortId dstNode then do
        dstPort <- getPort dstPortRef ne
        (srcPos, dstPos) <- Connection.connectionPositions srcNode srcPort dstNode dstPort (ne ^. layout)
        return $ PosConnection src dst srcPos dstPos sidebarConn mode (srcPort ^. Port.color)
    else if countArgPorts dstNode == getPortNumber dstPortId then case dstNode of
        Expression n -> fmap (Connection.toPosConnection src dstPortRef) $
            toPosHalfConnection ne $ HalfConnection (OutPortRef' src) (Connection.argConstructorPosition n) sidebarConn mode
        _            -> Nothing
    else Nothing


toPosHalfConnection :: NodeEditor -> HalfConnection -> Maybe PosHalfConnection
toPosHalfConnection ne halfConnection = do
    let src         = halfConnection ^. Connection.from
        dstPos      = halfConnection ^. Connection.dstPos
        pid         = src ^. PortRef.portId
        mode        = halfConnection ^. Connection.mode
        sidebarConn = halfConnection ^. Connection.sidebarConn
    node <- getNode (src ^. PortRef.nodeLoc) ne
    (srcPos, c) <-
        if hasPort pid node then do
            port   <- getPort src ne
            srcPos <- Connection.halfConnectionSrcPosition node (convert port) dstPos (ne ^. layout)
            return (srcPos, port ^. Port.color)
        else if countArgPorts node == getPortNumber pid then case node of
            Expression n -> return (Connection.argConstructorPosition n, Color 0)
            _            -> Nothing
        else Nothing
    return $ PosHalfConnection srcPos dstPos sidebarConn mode c

getVisualizations :: NodeEditor -> [VisualizationProperties]
getVisualizations ne = concatMap getVisualizationsForNode . Map.toList $ ne ^. nodeVisualizations where
    getVisualizationsForNode (nl, nv) = case getExpressionNode nl ne of
        Nothing -> def
        Just n  -> if not $ n ^. ExpressionNode.visualizationsEnabled then def else do
            let isExpanded  = ExpressionNode.isExpanded n
                argPortsNum = ExpressionNode.countArgPorts n
                visualizers = nv ^. Visualization.visualizers
            map (VisualizationProperties nl isExpanded argPortsNum visualizers) . Map.elems $ nv ^. Visualization.visualizations

isVisualizationNodeSelected :: VisualizationProperties -> NodeEditor -> Bool
isVisualizationNodeSelected visProp = maybe False (view ExpressionNode.isSelected) . getExpressionNode (visProp ^. Visualization.visPropNodeLoc)

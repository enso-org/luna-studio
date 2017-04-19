{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TypeFamilies           #-}
module Luna.Studio.React.Model.NodeEditor where

import qualified Data.HashMap.Strict                         as HashMap
import           Data.Position                               (Position)
import qualified Empire.API.Data.Breadcrumb                  as B
import           Empire.API.Data.MonadPath                   (MonadPath)
import qualified Empire.API.Data.NodeLoc                     as NodeLoc
import qualified Empire.API.Data.PortRef                     as PortRef
import           Luna.Studio.Data.CameraTransformation       (CameraTransformation)
import           Luna.Studio.Data.Color                      (Color (Color))
import           Luna.Prelude
import           Luna.Studio.React.Model.Connection          (Connection, ConnectionsMap, HalfConnection (HalfConnection),
                                                              PosConnection (PosConnection), PosHalfConnection (PosHalfConnection))
import qualified Luna.Studio.React.Model.Connection          as Connection
import           Luna.Studio.React.Model.ConnectionPen       (ConnectionPen)
import           Luna.Studio.React.Model.Layout              (Layout)
import qualified Luna.Studio.React.Model.Layout              as Layout
import           Luna.Studio.React.Model.Node                (ExpressionNode, ExpressionNodesMap, HasNodeLoc, InputNode,
                                                              Node (Expression, Input, Output), NodeLoc, OutputNode, countArgPorts, hasPort,
                                                              lookupPort, nodeId)
import qualified Luna.Studio.React.Model.Node.ExpressionNode as ExpressionNode
import           Luna.Studio.React.Model.Port                (AnyPort, AnyPortRef (OutPortRef'), InPort, InPortRef, OutPort, OutPortRef,
                                                              getPortNumber)
import qualified Luna.Studio.React.Model.Port                as Port
import           Luna.Studio.React.Model.Searcher            (Searcher)
import           Luna.Studio.React.Model.SelectionBox        (SelectionBox)


data NodeEditor = NodeEditor { _expressionNodes     :: ExpressionNodesMap
                             , _inputNode           :: Maybe InputNode
                             , _outputNode          :: Maybe OutputNode
                             , _monads              :: [MonadPath]
                             , _connections         :: ConnectionsMap
                             , _visualizations      :: [(NodeLoc, Int, Position)] --TODO move to node

                             , _halfConnections     :: [HalfConnection]
                             , _connectionPen       :: Maybe ConnectionPen
                             , _selectionBox        :: Maybe SelectionBox
                             , _searcher            :: Maybe Searcher

                             , _layout              :: Layout
                             } deriving (Default, Eq, Generic)

makeLenses ''NodeEditor

screenTransform :: Lens' NodeEditor CameraTransformation
screenTransform = layout . Layout.screenTransform

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

toPosConnection :: NodeEditor -> Connection -> Maybe PosConnection
toPosConnection ne connection = do
    let srcPortRef = connection ^. Connection.src
        dstPortRef = connection ^. Connection.dst
        srcNodeLoc = srcPortRef ^. PortRef.srcNodeLoc
        dstNodeLoc = dstPortRef ^. PortRef.dstNodeLoc
        dstPortId  = connection ^. Connection.dst . PortRef.dstPortId
    srcNode <- getNode srcNodeLoc ne
    dstNode <- getNode dstNodeLoc ne
    srcPort <- getPort srcPortRef ne
    if hasPort dstPortId dstNode then do
        dstPort <- getPort dstPortRef ne
        (srcPos, dstPos) <- Connection.connectionPositions srcNode srcPort dstNode dstPort (ne ^. layout)
        return $ PosConnection srcPortRef dstPortRef srcPos dstPos (connection ^. Connection.mode) (srcPort ^. Port.color)
    else if countArgPorts dstNode == getPortNumber dstPortId then case dstNode of
        Expression n -> fmap (Connection.toPosConnection srcPortRef dstPortRef) $
            toPosHalfConnection ne $ HalfConnection (OutPortRef' srcPortRef) (Connection.portPhantomPosition n) (connection ^. Connection.mode)
        _            -> Nothing
    else Nothing


toPosHalfConnection :: NodeEditor -> HalfConnection -> Maybe PosHalfConnection
toPosHalfConnection ne halfConnection = do
    let src    = halfConnection ^. Connection.from
        dstPos = halfConnection ^. Connection.dstPos
        pid    = src ^. PortRef.portId
    node                 <- getNode (src ^. PortRef.nodeLoc) ne
    (srcPos, t, c)  <-
        if hasPort pid node then do
            port   <- getPort src ne
            srcPos <- Connection.halfConnectionSrcPosition node (convert port) dstPos (ne ^. layout)
            return (srcPos, Connection.halfConnectionMode node, port ^. Port.color)
        else if countArgPorts node == getPortNumber pid then case node of
            Expression n -> return (Connection.portPhantomPosition n, Connection.Normal, Color 0)
            _            -> Nothing
        else Nothing
    return $ PosHalfConnection srcPos dstPos t c

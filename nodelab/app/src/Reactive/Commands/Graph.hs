module Reactive.Commands.Graph
    ( allNodes
    , allNodes'
    , connectionIdToWidgetId
    , focusNode
    , getPort
    , portRefToWidgetId
    , updateConnection
    , updateConnections
    , updateConnectionsForNodes
    , updateNodeZOrder
    ) where


import           Control.Monad.Trans.Maybe           (runMaybeT)
import qualified Data.HashMap.Lazy                   as HashMap
import           Data.Ord                            (comparing)
import qualified Data.Set                            as Set
import           Utils.Angle
import           Utils.PreludePlus
import           Utils.Vector                        (Vector2 (Vector2), lengthSquared)

import           React.Store                         (Ref, ref, widget)
import qualified React.Store                         as Store
import           React.Store.Node                    (Node)
import qualified React.Store.Node                    as Node

import           Object.UITypes
import           Object.Widget                       hiding (widget)
import qualified Object.Widget.Connection            as ConnectionModel
import qualified Object.Widget.Node                  as Model
import qualified Object.Widget.Port                  as PortModel

import           Reactive.Commands.Command           (Command)
import           Reactive.Commands.Node.Ports.Colors (vtToColor)
import qualified Reactive.Commands.UIRegistry        as UICmd
import           Reactive.State.Global               (inRegistry)
import qualified Reactive.State.Global               as Global
import qualified Reactive.State.Graph                as Graph
import           UI.Handlers.Node                    (allNodes, allNodes')
import           UI.Instances                        ()

import           Empire.API.Data.Connection          (ConnectionId)
import qualified Empire.API.Data.Connection          as Connection
import           Empire.API.Data.Node                (NodeId)
import qualified Empire.API.Data.Node                as NodeAPI
import qualified Empire.API.Data.Port                as Port
import           Empire.API.Data.PortRef             (AnyPortRef (..), InPortRef (..))
import qualified Empire.API.Data.PortRef             as PortRef



getPort :: AnyPortRef -> Command Global.State (Maybe PortModel.Port)
getPort portRef = runMaybeT $ do
    Just node <- lift $ getNode $ portRef ^. PortRef.nodeId
    fromJustM $ node ^? Node.ports . ix portRef

getGraphPort :: AnyPortRef -> Command Global.State (Maybe Port.Port)
getGraphPort portRef = preuse $ Global.graph . Graph.nodesMap . ix (portRef ^. PortRef.nodeId) . NodeAPI.ports . ix (portRef ^. PortRef.portId)

getNode :: NodeId -> Command Global.State (Maybe Model.Node)
getNode nodeId = Global.inNode nodeId $ mapM Store.get

connectionIdToWidgetId :: ConnectionId -> Command Global.State (Maybe WidgetId)
connectionIdToWidgetId connectionId = preuse $ Global.graph . Graph.connectionWidgetsMap . ix connectionId

portRefToWidgetId :: AnyPortRef -> Command Global.State (Maybe (Ref Node))
portRefToWidgetId = Global.getNode . view PortRef.nodeId

nats :: [Integer]
nats = [1..]

focusNode :: Ref Node -> Command Global.State ()
focusNode nodeRef = do
    node <- Store.get' nodeRef
    nodes <- mapM Store.get' =<< allNodes
    let sortedNodes = sortBy (comparing $ negate . (view $ widget . Model.zPos)) nodes
        equalFst a b = a ^. widget == b ^. widget
        newOrderNodes = node : deleteBy equalFst node sortedNodes
        newOrderRefs  = view ref <$> newOrderNodes
    forM_ (zip newOrderRefs nats) $ \(nRef, idx) -> do
        let newZPos = negate $ (fromIntegral idx) / 100.0
        Store.modify_ (Node.zPos .~ newZPos) nRef

updateNodeZOrder :: Command Global.State ()
updateNodeZOrder = do
    nodes <- mapM Store.get' =<< allNodes
    let sortedNodes = sortBy (comparing $ negate . (view $ widget . Model.zPos)) nodes
        sortedRefs  = view ref <$> sortedNodes
    forM_ (zip sortedRefs nats) $ \(nRef, idx) -> do
        let newZPos = negate $ (fromIntegral idx) / 100.0
        Store.modify_ (Node.zPos .~ newZPos) nRef

updateConnections :: Command Global.State ()
updateConnections = do
    connectionIds <- uses (Global.graph . Graph.connectionsMap) HashMap.keys
    mapM_ updateConnection connectionIds

updateConnectionsForNodes :: [NodeId] -> Command Global.State ()
updateConnectionsForNodes nodes = do
    connections <- uses (Global.graph . Graph.connectionsMap) HashMap.toList
    let nodes' = Set.fromList nodes
        connectionsToUpdate = [wid | (wid, conn) <- connections, (    (conn ^. Connection.src . PortRef.srcNodeId) `Set.member` nodes'
                                                                 || (conn ^. Connection.dst . PortRef.dstNodeId) `Set.member` nodes') ]
    mapM_ updateConnection connectionsToUpdate

lineEndPos :: Vector2 Double -> Vector2 Double -> Double -> Maybe PortModel.Port -> Vector2 Double
lineEndPos node1Pos node2Pos radius (Just port) = moveByAngle node1Pos radius portAngle' where
    portAngle   = port ^. PortModel.angle
    portCount   = port ^. PortModel.portCount
    portAngle'  = boundedAngle portAngle portCount node1Pos node2Pos
lineEndPos node1Pos node2Pos radius Nothing = moveByAngle node1Pos radius portAngle where
    portAngle = toAngle $ node2Pos - node1Pos

--

updateConnection :: ConnectionId -> Command Global.State () -- FIXME: run in MaybeT
updateConnection connectionId = do
    (Just connection) <- preuse $ Global.graph . Graph.connectionsMap       . ix connectionId -- fatal
    (Just widgetId  ) <- preuse $ Global.graph . Graph.connectionWidgetsMap . ix connectionId -- fatal

    (Just srcNode   ) <- getNode $ connection ^. Connection.src . PortRef.srcNodeId           -- fatal
    (Just dstNode   ) <- getNode $ connection ^. Connection.dst . PortRef.dstNodeId           -- fatal

    srcPort           <- getPort      $ OutPortRef' $ connection ^. Connection.src            -- non-fatal
    dstPort           <- getPort      $ InPortRef'  $ connection ^. Connection.dst            -- non-fatal
    let dstNodePos     = dstNode ^. widgetPosition
        srcNodePos     = srcNode ^. widgetPosition
        dstRadius      = portRadius $ connection ^. Connection.dst
        posSrc         = lineEndPos srcNodePos dstNodePos normalPortRadius srcPort
        posDst         = lineEndPos dstNodePos srcNodePos dstRadius        dstPort

    srcGraphPort      <- getGraphPort $ OutPortRef' $ connection ^. Connection.src            -- non-fatal
    let visible        = lengthSquared (dstNodePos - srcNodePos) > 100
        fallbackColor  = 13
        color          = fromMaybe fallbackColor $ vtToColor <$> (view Port.valueType) <$> srcGraphPort

    void $ inRegistry $ UICmd.update widgetId $ (ConnectionModel.from    .~ posSrc)
                                              . (ConnectionModel.to      .~ posDst)
                                              . (ConnectionModel.visible .~ visible)
                                              . (ConnectionModel.color   .~ color)

moveByAngle :: Vector2 Double -> Double -> Angle -> Vector2 Double
moveByAngle (Vector2 x y) radius angle = Vector2 (x + radius * cos angle) (y + radius * sin angle)

normalPortRadius :: Double
normalPortRadius = 22.0

portRadius :: InPortRef -> Double
portRadius (InPortRef _ Port.Self) = 0.0
portRadius _ = normalPortRadius

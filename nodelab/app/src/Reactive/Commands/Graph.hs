module Reactive.Commands.Graph
    ( portRefToWidgetId
    , updateConnections
    , updateConnection
    , updateConnectionsForNodes
    , connectionIdToWidgetId
    , allNodes
    , widgetIdToNodeWidget
    , nodeIdToWidgetId
    , focusNode
    , updateNodeZOrder
    ) where


import           Control.Monad.Trans.Maybe           (runMaybeT)
import qualified Data.HashMap.Lazy                   as HashMap
import           Data.Ord                            (comparing)
import qualified Data.Set                            as Set
import           Utils.Angle
import           Utils.PreludePlus
import           Utils.Vector

import           Object.UITypes
import           Object.Widget
import qualified Object.Widget.Connection            as ConnectionModel
import qualified Object.Widget.Node                  as Model
import qualified Object.Widget.Port                  as PortModel

import           Reactive.Commands.Command           (Command)
import           Reactive.Commands.Node.Ports.Colors (vtToColor)
import qualified Reactive.Commands.UIRegistry        as UICmd
import           Reactive.State.Global               (inRegistry)
import qualified Reactive.State.Global               as Global
import qualified Reactive.State.Graph                as Graph
import qualified Reactive.State.UIRegistry           as UIRegistry
import           UI.Handlers.Node                    (allNodes)
import           UI.Instances                        ()

import           Empire.API.Data.Connection          (ConnectionId)
import qualified Empire.API.Data.Connection          as Connection
import           Empire.API.Data.Node                (NodeId)
import qualified Empire.API.Data.Node                as Node
import qualified Empire.API.Data.Port                as Port
import           Empire.API.Data.PortRef             (AnyPortRef (..), InPortRef (..))
import qualified Empire.API.Data.PortRef             as PortRef

-- allPorts :: Command Global.State [WidgetFile PortModel.Port]
-- allPorts = do
--     widgetIds <- use $ Global.graph . Graph.portWidgets
--     mayWidgets <- mapM (\wid -> inRegistry $ UIRegistry.lookupTypedM wid) widgetIds
--     return $ catMaybes mayWidgets

getPort :: AnyPortRef -> Command Global.State (Maybe PortModel.Port)
getPort portRef = runMaybeT $ do
    (Just widgetId) <- preuse $ Global.graph . Graph.portWidgetsMap . ix portRef
    lift $ inRegistry $ UICmd.lookup widgetId

getGraphPort :: AnyPortRef -> Command Global.State (Maybe Port.Port)
getGraphPort portRef = preuse $ Global.graph . Graph.nodesMap . ix (portRef ^. PortRef.nodeId) . Node.ports . ix (portRef ^. PortRef.portId)

getNode :: NodeId -> Command Global.State (Maybe Model.Node)
getNode nodeId = runMaybeT $ do
    (Just widgetId)   <- preuse $ Global.graph . Graph.nodeWidgetsMap . ix nodeId
    lift $ inRegistry $ UICmd.lookup widgetId

widgetIdToNodeWidget :: WidgetId -> Command UIRegistry.State (Maybe (WidgetFile Model.Node))
widgetIdToNodeWidget = UIRegistry.lookupTypedM

nodeIdToWidgetId :: NodeId -> Command Global.State (Maybe WidgetId)
nodeIdToWidgetId nodeId = preuse $ Global.graph . Graph.nodeWidgetsMap . ix nodeId

connectionIdToWidgetId :: ConnectionId -> Command Global.State (Maybe WidgetId)
connectionIdToWidgetId connectionId = preuse $ Global.graph . Graph.connectionWidgetsMap . ix connectionId

portRefToWidgetId :: AnyPortRef -> Command Global.State (Maybe WidgetId)
portRefToWidgetId portRef = preuse $ Global.graph . Graph.portWidgetsMap . ix portRef

nats :: [Integer]
nats = [1..]

focusNode :: WidgetId -> Command Global.State ()
focusNode wid = do
    nodes <- allNodes
    let sortedNodes = sortBy (comparing $ negate . (view $ widget . Model.zPos)) nodes
        sortedIds   = (view objectId) <$> sortedNodes
        newOrder    = wid : (delete wid sortedIds)
    inRegistry $ forM_ (zip newOrder nats) $ \(wid, ix) -> do
        let newZPos = negate $ (fromIntegral ix) / 100.0
        UICmd.update wid $ Model.zPos .~ newZPos

updateNodeZOrder :: Command Global.State ()
updateNodeZOrder = do
    nodes <- allNodes
    let sortedNodes = sortBy (comparing $ negate . (view $ widget . Model.zPos)) nodes
        sortedIds   = (view objectId) <$> sortedNodes
    inRegistry $ forM_ (zip sortedIds nats) $ \(wid, ix) -> do
        let newZPos = negate $ (fromIntegral ix) / 100.0
        UICmd.update wid $ Model.zPos .~ newZPos

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

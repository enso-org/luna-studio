{-# OPTIONS_GHC -fno-warn-orphans #-}
module Luna.Studio.Action.Connect.DragConnect
    ( connectToPort
    , handleMove
    , modifyConnection
    , startOrModifyConnection
    , stopDrag
    , whileConnecting
    ) where

import qualified Data.HashMap.Strict                    as HashMap
import           Empire.API.Data.Connection             (Connection)
import           Empire.API.Data.Connection             (ConnectionId)
import qualified Empire.API.Data.Connection             as Connection
import           Empire.API.Data.PortRef                (AnyPortRef (InPortRef', OutPortRef'))
import qualified JS.GoogleAnalytics                     as GA
import           Luna.Studio.Action.Command             (Command)
import           Luna.Studio.Action.Connect.Color       (getConnectionColor)
import           Luna.Studio.Action.Connect.Helpers     (createCurrentConnection, toValidConnection)
import           Luna.Studio.Action.Geometry.Connection (getCurrentConnectionPosition)
import           Luna.Studio.Action.Graph.Connect       (connectNodes, localConnectNodes)
import           Luna.Studio.Action.Graph.Disconnect    (removeConnections)
import           Luna.Studio.Event.Mouse                (workspacePosition)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Event.Connection     (ModifiedEnd (Destination, Source))
import           Luna.Studio.React.Model.Connection     (CurrentConnection)
import qualified Luna.Studio.React.Model.Connection     as ConnectionModel
import qualified Luna.Studio.React.Model.NodeEditor     as NodeEditor
import           Luna.Studio.State.Action               (Action (Connect))
import qualified Luna.Studio.State.Connect              as Connect
import           Luna.Studio.State.Global               (State)
import qualified Luna.Studio.State.Global               as Global
import qualified Luna.Studio.State.Graph                as Graph
import           Luna.Studio.State.StatefulAction       (StatefulAction (exit, matchState, pack, start))
import           React.Flux                             (MouseEvent)



instance StatefulAction Connect.State where
    matchState (Connect state) = Just state
    matchState _ = Nothing
    pack = Connect
    exit _ = whileConnecting stopDrag

startOrModifyConnection :: MouseEvent -> AnyPortRef -> Command State ()
startOrModifyConnection evt anyPortRef = case anyPortRef of
    InPortRef'  portRef -> do
        portConnected <- HashMap.member portRef <$> (use $ Global.graph . Graph.connectionsMap)
        if portConnected then
            modifyConnection evt portRef Destination
        else startDragFromPort evt anyPortRef Nothing
    OutPortRef' _ -> startDragFromPort evt anyPortRef Nothing

startDragFromPort :: MouseEvent -> AnyPortRef -> Maybe Connection -> Command State ()
startDragFromPort evt portRef modifiedConnection = do
    start $ Connect.State
    mousePos  <- workspacePosition evt
    maySrcPos <- getCurrentConnectionPosition portRef mousePos
    mayColor  <- getConnectionColor portRef
    withJust ((,) <$> maySrcPos <*> mayColor) $ \((srcPos, dstPos), color) -> do
        createCurrentConnection portRef modifiedConnection srcPos dstPos color

modifyConnection :: MouseEvent -> ConnectionId -> ModifiedEnd -> Command State ()
modifyConnection evt connId modifiedEnd = do
    mayConn <- preuse $ Global.graph . Graph.connectionsMap . ix connId
    withJust mayConn $ \conn -> do
        let portRef = case modifiedEnd of
                Source      -> InPortRef'  (conn ^. Connection.dst)
                Destination -> OutPortRef' (conn ^. Connection.src)
        startDragFromPort evt portRef $ Just conn

whileConnecting :: (CurrentConnection -> Command State ()) -> Command State ()
whileConnecting run = do
    mayCurrentConnection <- view NodeEditor.currentConnection <$> Global.getNodeEditor
    withJust mayCurrentConnection run

handleMove :: MouseEvent -> CurrentConnection -> Command State ()
handleMove evt conn = do
    mousePos   <- workspacePosition evt
    let srcPortRef = conn ^. ConnectionModel.srcPortRef
    maySrcPos  <- getCurrentConnectionPosition srcPortRef mousePos
    case maySrcPos of
        Just (srcPos, dstPos) -> Global.modifyCurrentConnection $ do
            ConnectionModel.currentTo   .= dstPos
            ConnectionModel.currentFrom .= srcPos
        Nothing               -> stopDrag conn

stopDrag :: CurrentConnection -> Command State ()
stopDrag conn = do
    Global.modifyNodeEditor $ NodeEditor.currentConnection .= Nothing
    Global.performedAction .= Nothing
    let mayModifiedConnection = conn ^. ConnectionModel.modifiedConnection
    withJust mayModifiedConnection $ \modifiedConnection -> do
        removeConnections [modifiedConnection ^. Connection.dst]

connectToPort :: AnyPortRef -> CurrentConnection -> Command State ()
connectToPort dstPortRef conn = do
    Global.modifyNodeEditor $ NodeEditor.currentConnection .= Nothing
    Global.performedAction .= Nothing
    let srcPortRef            = conn ^. ConnectionModel.srcPortRef
        mayModifiedConnection = conn ^. ConnectionModel.modifiedConnection
    withJust (toValidConnection srcPortRef dstPortRef) $ \(src, dst) -> case mayModifiedConnection of
        Just prevConn -> do
            if src == prevConn ^. Connection.src && dst == prevConn ^. Connection.dst then
                void $ localConnectNodes src dst
            else do
                removeConnections [prevConn ^. Connection.dst]
                connectNodes src dst
                GA.sendEvent $ GA.Connect GA.Manual
        _ -> do
            connectNodes src dst
            GA.sendEvent $ GA.Connect GA.Manual

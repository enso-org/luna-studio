module Luna.Studio.Action.Connect.DragConnect
    ( connectToPort
    , handleMove
    , modifyConnection
    , startDragFromPort
    , stopDrag
    , whileConnecting
    ) where

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
import qualified Luna.Studio.React.Store                as Store
import           Luna.Studio.React.Store.Ref            (Ref)
import qualified Luna.Studio.React.Store.Ref            as Ref
import           Luna.Studio.State.Global               (State)
import qualified Luna.Studio.State.Global               as Global
import qualified Luna.Studio.State.Graph                as Graph
import           React.Flux                             (MouseEvent)


startDragFromPort :: MouseEvent -> AnyPortRef -> Maybe Connection -> Command State ()
startDragFromPort evt portRef modifiedConnection = do
    mousePos  <- workspacePosition evt
    maySrcPos <- getCurrentConnectionPosition portRef mousePos
    mayColor  <- getConnectionColor portRef
    withJust ((,) <$> maySrcPos <*> mayColor) $ \((srcPos, dstPos), color) -> do
        createCurrentConnection portRef modifiedConnection srcPos dstPos color

whileConnecting :: (Ref CurrentConnection -> Command State ()) -> Command State ()
whileConnecting run = do
    mayCurrentConnectionRef <- Global.withNodeEditor $ Store.use NodeEditor.currentConnection
    withJust mayCurrentConnectionRef $ \currentConnectionRef -> run currentConnectionRef

handleMove :: MouseEvent -> Ref CurrentConnection -> Command State ()
handleMove evt connRef = do
    mousePos   <- workspacePosition evt
    srcPortRef <- view ConnectionModel.srcPortRef <$> Ref.get connRef
    maySrcPos  <- getCurrentConnectionPosition srcPortRef mousePos
    case maySrcPos of
        Just (srcPos, dstPos) -> flip Store.modifyM_ connRef $ do
            ConnectionModel.currentTo   .= dstPos
            ConnectionModel.currentFrom .= srcPos
        Nothing               -> stopDrag connRef

stopDrag :: Ref CurrentConnection -> Command State ()
stopDrag connRef = do
    Global.withNodeEditor $ Store.modifyM_ $ NodeEditor.currentConnection .= Nothing
    mayModifiedConnection <- view ConnectionModel.modifiedConnection <$> Ref.get connRef
    withJust mayModifiedConnection $ \conn -> do
        removeConnections [conn ^. Connection.dst]

connectToPort :: AnyPortRef -> Ref CurrentConnection -> Command State ()
connectToPort dstPortRef connRef = do
    Global.withNodeEditor $ Store.modifyM_ $ NodeEditor.currentConnection .= Nothing
    srcPortRef <- view ConnectionModel.srcPortRef <$> Ref.get connRef
    mayModifiedConnection <- view ConnectionModel.modifiedConnection <$> Ref.get connRef
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


modifyConnection :: MouseEvent -> ConnectionId -> ModifiedEnd -> Command State ()
modifyConnection evt connId modifiedEnd = do
    mayConn <- preuse $ Global.graph . Graph.connectionsMap . ix connId
    withJust mayConn $ \conn -> do
        let portRef = case modifiedEnd of
                Source      -> InPortRef'  (conn ^. Connection.dst)
                Destination -> OutPortRef' (conn ^. Connection.src)
        startDragFromPort evt portRef $ Just conn

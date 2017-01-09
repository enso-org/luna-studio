module Luna.Studio.Commands.Connect
    ( connectToPort
    , handleMove
    , modifyConnection
    , startDragFromPort
    , stopDrag
    , whileConnecting
    ) where

import qualified Data.HashMap.Strict                   as Map
import           Empire.API.Data.Connection            (Connection)
import           Empire.API.Data.Connection            (ConnectionId)
import qualified Empire.API.Data.Connection            as Connection
import           Empire.API.Data.PortRef               (AnyPortRef (InPortRef', OutPortRef'), InPortRef (InPortRef),
                                                        OutPortRef (OutPortRef))
import qualified Empire.API.Data.PortRef               as PortRef
import qualified JS.GoogleAnalytics                    as GA
import           Luna.Studio.Commands.Command          (Command)
import           Luna.Studio.Commands.Graph.Connect    (connectNodes, localConnectNodes)
import           Luna.Studio.Commands.Graph.Disconnect (disconnect, localDisconnect)
import           Luna.Studio.Data.Color                (Color)
import           Luna.Studio.Data.Vector               (Position)
import           Luna.Studio.Event.Mouse               (workspacePosition)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Event.Connection    (ModifiedEnd (Destination, Source))
import           Luna.Studio.React.Model.Connection    (CurrentConnection (CurrentConnection))
import qualified Luna.Studio.React.Model.NodeEditor    as NodeEditor
import qualified Luna.Studio.React.Store               as Store
import           Luna.Studio.React.Store.Ref           (Ref)
import qualified Luna.Studio.React.Store.Ref           as Ref
import           Luna.Studio.React.View.Connection     (getConnectionColor, getConnectionSrcPosition)
import           Luna.Studio.State.Global              (State)
import qualified Luna.Studio.State.Global              as Global
import qualified Luna.Studio.State.Graph               as Graph
import qualified Object.Widget.Connection              as ConnectionModel
import           React.Flux                            (MouseEvent)


createCurrentConnection :: AnyPortRef -> Maybe Connection -> Position -> Position -> Color -> Command State ()
createCurrentConnection portRef modifiedConn srcPos dstPos color = do
    withJust modifiedConn $ \conn -> localDisconnect [conn ^. Connection.dst]
    let connection = CurrentConnection portRef modifiedConn srcPos dstPos color
    Global.withNodeEditor $ Store.modifyM_ $ do
        connectionRef <- lift $ Store.create connection
        NodeEditor.currentConnection ?= connectionRef

startDragFromPort :: MouseEvent -> AnyPortRef -> Maybe Connection -> Command State ()
startDragFromPort evt portRef modifiedConnection = do
    mousePos  <- workspacePosition evt
    maySrcPos <- getConnectionSrcPosition portRef mousePos
    mayColor  <- getConnectionColor portRef
    withJust ((,) <$> maySrcPos <*> mayColor) $ \(srcPos, color) -> do
        createCurrentConnection portRef modifiedConnection srcPos mousePos color

whileConnecting :: (Ref CurrentConnection -> Command State ()) -> Command State ()
whileConnecting run = do
    mayCurrentConnectionRef <- Global.withNodeEditor $ Store.use NodeEditor.currentConnection
    withJust mayCurrentConnectionRef $ \currentConnectionRef -> run currentConnectionRef

handleMove :: MouseEvent -> Ref CurrentConnection -> Command State ()
handleMove evt connRef = do
    mousePos   <- workspacePosition evt
    srcPortRef <- view ConnectionModel.srcPortRef <$> Ref.get connRef
    maySrcPos  <- getConnectionSrcPosition srcPortRef mousePos
    case maySrcPos of
        Just pos -> flip Store.modifyM_ connRef $ do
            ConnectionModel.currentTo .= mousePos
            ConnectionModel.currentFrom .= pos
        Nothing  -> stopDrag connRef

stopDrag :: Ref CurrentConnection -> Command State ()
stopDrag connRef = do
    Global.withNodeEditor $ Store.modifyM_ $ NodeEditor.currentConnection .= Nothing
    mayModifiedConnection <- view ConnectionModel.modifiedConnection <$> Ref.get connRef
    withJust mayModifiedConnection $ \conn -> do
        disconnect [conn ^. Connection.dst]

toValidConnection :: AnyPortRef -> AnyPortRef -> Maybe (OutPortRef, InPortRef)
toValidConnection src' dst' = (normalize' src' dst') >>= toOtherNode where
    normalize' (OutPortRef' a) (InPortRef' b) = Just (a, b)
    normalize' (InPortRef' a) (OutPortRef' b) = Just (b, a)
    normalize' _ _ = Nothing
    toOtherNode (a, b)
        | a ^. PortRef.srcNodeId /= b ^. PortRef.dstNodeId = Just (a, b)
        | otherwise                                        = Nothing

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
                disconnect [prevConn ^. Connection.dst]
                connectNodes src dst
                GA.sendEvent $ GA.Connect GA.Manual
        _ -> do
            connectNodes src dst
            GA.sendEvent $ GA.Connect GA.Manual


modifyConnection :: MouseEvent -> ConnectionId -> ModifiedEnd -> Command State ()
modifyConnection evt connId modifiedEnd = do
    mayConn <- preuse $ Global.graph . Graph.connectionsMap . ix connId
    withJust mayConn $ \conn -> do
        mousePos  <- workspacePosition evt
        let portRef = case modifiedEnd of
                Source      -> InPortRef'  (conn ^. Connection.dst)
                Destination -> OutPortRef' (conn ^. Connection.src)
        startDragFromPort evt portRef $ Just conn

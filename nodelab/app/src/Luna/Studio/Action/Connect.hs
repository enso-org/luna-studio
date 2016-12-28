module Luna.Studio.Action.Connect
    ( toAction
    ) where
--TODO[react]: transform mousePos to correct position

import           Empire.API.Data.Connection         (ConnectionId)
import           Empire.API.Data.PortRef            (AnyPortRef (InPortRef', OutPortRef'), InPortRef (InPortRef), OutPortRef (OutPortRef))
import qualified Empire.API.Data.PortRef            as PortRef
import           Event.Event                        (Event (UI))
import           Event.UI                           (UIEvent (AppEvent, ConnectionEvent))
import qualified JS.GoogleAnalytics                 as GA
import           Luna.Studio.Commands.Command       (Command)
import           Luna.Studio.Commands.Graph.Connect (batchConnectNodes)
import           Luna.Studio.Data.Vector            (Position)
import           Luna.Studio.Event.Mouse            (workspacePosition)
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Event.App        as App
import qualified Luna.Studio.React.Event.Connection as Connection
import           Luna.Studio.React.Model.Connection (CurrentConnection)
import qualified Luna.Studio.React.Model.NodeEditor as NodeEditor
import qualified Luna.Studio.React.Store            as Store
import           Luna.Studio.React.Store.Ref        (Ref)
import qualified Luna.Studio.React.Store.Ref        as Ref
import           Luna.Studio.React.View.Global      (getCurrentConnectionColor, getCurrentConnectionSrcPosition)
import           Luna.Studio.State.Global           (State)
import qualified Luna.Studio.State.Global           as Global
import qualified Object.Widget.Connection           as ConnectionModel
import           React.Flux                         (MouseEvent)


toAction :: Event -> Maybe (Command State ())
toAction (UI (ConnectionEvent (Connection.StartConnection evt portRef))) = Just $ startDragFromPort evt portRef
toAction (UI (AppEvent  (App.MouseMove evt)))                            = Just $ whileConnecting $ handleMove evt
toAction (UI (AppEvent (App.MouseUp _)))                                 = Just $ whileConnecting $ stopDrag'
toAction (UI (ConnectionEvent (Connection.EndConnection _ portRef)))     = Just $ whileConnecting $ stopDrag portRef
toAction (UI (ConnectionEvent (Connection.ModifyConnection evt connId))) = Just $ modifyConnection evt connId
toAction _                                                               = Nothing


startDragFromPort :: MouseEvent -> AnyPortRef -> Command State ()
startDragFromPort evt portRef = do
    mousePos  <- workspacePosition evt
    maySrcPos <- getCurrentConnectionSrcPosition portRef mousePos
    withJust maySrcPos $ \srcPos -> do
        mayColor  <- getCurrentConnectionColor portRef
        withJust mayColor $ \color -> do
            let connection = ConnectionModel.CurrentConnection portRef True srcPos mousePos False color
            Global.withNodeEditor $ Store.modifyM_ $ do
                connectionRef <- lift $ Store.create connection
                NodeEditor.currentConnection ?= connectionRef

whileConnecting :: (Ref CurrentConnection -> Command State ()) -> Command State ()
whileConnecting run = do
    mayCurrentConnectionRef <- Global.withNodeEditor $ Store.use NodeEditor.currentConnection
    withJust mayCurrentConnectionRef $ \currentConnectionRef -> run currentConnectionRef

handleMove :: MouseEvent -> Ref CurrentConnection -> Command State ()
handleMove evt connRef = do
    mousePos   <- workspacePosition evt
    srcPortRef <- view ConnectionModel.srcPortRef <$> Ref.get connRef
    maySrcPos  <- getCurrentConnectionSrcPosition srcPortRef mousePos
    case maySrcPos of
        Just pos -> flip Store.modifyM_ connRef $ do
            ConnectionModel.currentTo .= mousePos
            ConnectionModel.currentFrom .= pos
        Nothing  -> stopDrag' connRef

stopDrag' :: Ref CurrentConnection -> Command State ()
stopDrag' _ = Global.withNodeEditor $ Store.modifyM_ $ NodeEditor.currentConnection .= Nothing

toValidConnection :: AnyPortRef -> AnyPortRef -> Maybe (OutPortRef, InPortRef)
toValidConnection src' dst' = (normalize' src' dst') >>= toOtherNode where
    normalize' (OutPortRef' a) (InPortRef' b) = Just (a, b)
    normalize' (InPortRef' a) (OutPortRef' b) = Just (b, a)
    normalize' _ _ = Nothing
    toOtherNode (a, b)
        | a ^. PortRef.srcNodeId /= b ^. PortRef.dstNodeId = Just (a, b)
        | otherwise                                        = Nothing

stopDrag :: AnyPortRef -> Ref CurrentConnection -> Command State ()
stopDrag dstPortRef connRef = do
    Global.withNodeEditor $ Store.modifyM_ $ NodeEditor.currentConnection .= Nothing
    srcPortRef <- view ConnectionModel.srcPortRef <$> Ref.get connRef
    withJust (toValidConnection srcPortRef dstPortRef) $ \(src, dst) -> do
        batchConnectNodes src dst
        GA.sendEvent $ GA.Connect GA.Manual

--TODO[react]: Implement this function
modifyConnection :: MouseEvent -> ConnectionId -> Command State ()
modifyConnection evt connId = return ()

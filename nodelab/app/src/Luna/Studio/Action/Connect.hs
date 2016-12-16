module Luna.Studio.Action.Connect
    ( toAction
    ) where

import           Empire.API.Data.Node               (NodeId)
import           Empire.API.Data.Port               (PortId)
import           Empire.API.Data.PortRef            (toAnyPortRef)
import           Empire.API.Data.PortRef            (AnyPortRef (..), InPortRef (..), OutPortRef (..))
import qualified Empire.API.Data.PortRef            as PortRef
import           Event.Event                        (Event (UI))
import           Event.UI                           (UIEvent (AppEvent, NodeEvent))
import qualified JS.GoogleAnalytics                 as GA
import           Luna.Studio.Commands.Command       (Command)
import           Luna.Studio.Commands.Graph.Connect (batchConnectNodes)
import           Luna.Studio.Data.Vector
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Event.App        as App
import qualified Luna.Studio.React.Event.Node       as Node
import           Luna.Studio.React.Model.Connection (CurrentConnection)
import qualified Luna.Studio.React.Model.Node       as NodeModel
import qualified Luna.Studio.React.Model.NodeEditor as NodeEditor
import qualified Luna.Studio.React.Store            as Store
import           Luna.Studio.React.Store.Ref        (Ref)
import qualified Luna.Studio.React.Store.Ref        as Ref
import qualified Luna.Studio.State.Camera           as Camera
import           Luna.Studio.State.Global           (State)
import qualified Luna.Studio.State.Global           as Global
import qualified Object.Widget.Connection           as ConnectionModel
import           React.Flux                         (mouseScreenX, mouseScreenY)


toAction :: Event -> Maybe (Command State ())
toAction (UI (NodeEvent (Node.StartConnection nodeId portId))) = Just $ startDragFromPort nodeId portId
toAction (UI (AppEvent  (App.MouseMove evt)))                  = Just $ whileConnecting $ handleMove pos
    where pos = Vector2 (mouseScreenX evt) (mouseScreenY evt)
toAction (UI (AppEvent (App.MouseUp _)))                       = Just $ whileConnecting $ stopDrag'
toAction (UI (NodeEvent (Node.EndConnection nodeId portId)))   = Just $ whileConnecting $ stopDrag nodeId portId
toAction _                                                     = Nothing


startDragFromPort :: NodeId -> PortId -> Command State ()
startDragFromPort nodeId portId = do
    maySrcNodeRef <- Global.getNode nodeId
    withJust maySrcNodeRef $ \srcNodeRef -> do
        let portRef = toAnyPortRef nodeId portId
        from' <- view NodeModel.position <$> Store.get srcNodeRef
        let connection = ConnectionModel.CurrentConnection portRef True from' from' False  def
        Global.withNodeEditor $ Store.modifyM_ $ do
            connectionRef <- lift $ Store.create connection
            NodeEditor.currentConnection ?= connectionRef

whileConnecting :: (Ref CurrentConnection -> Command State ()) -> Command State ()
whileConnecting run = do
    mayCurrentConnectionRef <- Global.withNodeEditor $ Store.use NodeEditor.currentConnection
    withJust mayCurrentConnectionRef $ \currentConnectionRef -> run currentConnectionRef

handleMove :: Vector2 Int -> Ref CurrentConnection -> Command State ()
handleMove (Vector2 x' y') connRef = do
    --TODO[react]: temp solution in next line
    to' <- zoom Global.camera $ Camera.screenToWorkspaceM (Vector2 (x'+200) y')
    flip Store.modifyM_ connRef $ do
        ConnectionModel.currentTo .= to'

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

stopDrag :: NodeId -> PortId -> Ref CurrentConnection -> Command State ()
stopDrag nodeId portId connRef = do
    Global.withNodeEditor $ Store.modifyM_ $ NodeEditor.currentConnection .= Nothing
    srcPortRef <- view ConnectionModel.srcPortRef <$> Ref.get connRef
    let dstPortRef = toAnyPortRef nodeId portId
        validConn  = toValidConnection srcPortRef dstPortRef
    withJust validConn $ \(src, dst) -> do
        batchConnectNodes src dst
        GA.sendEvent $ GA.Connect GA.Manual

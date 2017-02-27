{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeApplications #-}
module Luna.Studio.Action.Connect
    ( handleConnectionMouseDown
    , handleMove
    , handleMouseUp
    , handlePortMouseUp
    , startConnecting
    , snapToPort
    , cancelSnapToPort
    , connectToPort
    , stopConnecting
    ) where

import qualified Data.HashMap.Strict                    as HashMap
import           Data.ScreenPosition                    (ScreenPosition)
import           Empire.API.Data.Connection             (ConnectionId, toValidConnection)
import qualified Empire.API.Data.Connection             as Connection
import           Empire.API.Data.Port                   (InPort (Self), PortId (InPortId))
import           Empire.API.Data.PortRef                (AnyPortRef (InPortRef', OutPortRef'))
import qualified Empire.API.Data.PortRef                as PortRef
import qualified JS.GoogleAnalytics                     as GA
import           Luna.Studio.Action.Camera.Screen       (translateToWorkspace)
import           Luna.Studio.Action.Command             (Command)
import           Luna.Studio.Action.Geometry.Connection (createConnectionModel, createCurrentConnectionModel)
import           Luna.Studio.Action.Graph.Connect       (connectNodes)
import           Luna.Studio.Action.Graph.Disconnect    (removeConnections)
import           Luna.Studio.Action.Node.Drag           (startNodeDrag)
import           Luna.Studio.Action.Port.Self           (showOrHideAllSelfPorts)
import           Luna.Studio.Event.Mouse                (mousePosition, workspacePosition)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Event.Connection     (ModifiedEnd (Destination, Source))
import           Luna.Studio.React.Model.Connection     (toCurrentConnection)
import qualified Luna.Studio.React.Model.Node           as Model
import qualified Luna.Studio.React.Model.NodeEditor     as NodeEditor
import           Luna.Studio.State.Action               (Action (begin, continue, update), Connect, Mode (Click, Drag), connectAction)
import qualified Luna.Studio.State.Action               as Action
import           Luna.Studio.State.Global               (State, beginActionWithKey, continueActionWithKey, removeActionFromState,
                                                         updateActionWithKey)
import qualified Luna.Studio.State.Global               as Global
import qualified Luna.Studio.State.Graph                as Graph
import           React.Flux                             (MouseEvent)


instance Action (Command State) Connect where
    begin    = beginActionWithKey    connectAction
    continue = continueActionWithKey connectAction
    update   = updateActionWithKey   connectAction
    end      = stopConnecting


handleConnectionMouseDown :: MouseEvent -> ConnectionId -> ModifiedEnd -> Command State ()
handleConnectionMouseDown evt connId modifiedEnd = do
    connectionsMap <- use $ Global.graph . Graph.connectionsMap
    withJust (HashMap.lookup connId connectionsMap) $ \connection -> do
        let portRef = case modifiedEnd of
                Destination -> OutPortRef' (connection ^. Connection.src)
                Source      -> InPortRef'  (connection ^. Connection.dst)
        mousePos <- mousePosition evt
        startConnecting mousePos portRef (Just connId) Drag

startConnecting :: ScreenPosition -> AnyPortRef -> Maybe ConnectionId -> Mode -> Command State ()
startConnecting screenMousePos anyPortRef mayModifiedConnId connectMode = do
    let nodeId = anyPortRef ^. PortRef.nodeId
        portId = anyPortRef ^. PortRef.portId
    mousePos <- translateToWorkspace screenMousePos
    mayNode  <- Global.getNode nodeId
    withJust mayNode $ \node -> do
        case (portId, not $ node ^. Model.isCollapsed) of
            (InPortId Self, False) -> when (connectMode == Drag) $ startNodeDrag mousePos nodeId True
            _                      -> do
                mayCurrentConnectionModel <- createCurrentConnectionModel anyPortRef mousePos
                when (isJust mayCurrentConnectionModel) $ do
                    let action = Action.Connect screenMousePos anyPortRef (isJust mayModifiedConnId) Nothing connectMode
                    withJust mayModifiedConnId $ removeConnections . replicate 1
                    begin action
                    showOrHideAllSelfPorts (Just action) Nothing
                    Global.modifyNodeEditor $ do
                        withJust mayModifiedConnId $ \connId ->
                            NodeEditor.connections . at connId .= Nothing
                        NodeEditor.currentConnection .= mayCurrentConnectionModel

handleMove :: MouseEvent -> Connect -> Command State ()
handleMove evt action = when (isNothing $ action ^. Action.connectSnappedPort) $ do
    mousePos                  <- workspacePosition evt
    mayCurrentConnectionModel <- createCurrentConnectionModel (action ^. Action.connectSourcePort) mousePos
    Global.modifyNodeEditor $ NodeEditor.currentConnection .= mayCurrentConnectionModel
    when (isNothing mayCurrentConnectionModel) $ stopConnecting action

handlePortMouseUp :: AnyPortRef -> Connect -> Command State ()
handlePortMouseUp portRef action = when (action ^. Action.connectMode == Drag) $
    connectToPort portRef action

snapToPort :: AnyPortRef -> Connect -> Command State ()
snapToPort portRef action =
    withJust (toValidConnection (action ^. Action.connectSourcePort) portRef) $ \conn -> do
        mayConnModel <- createConnectionModel conn
        withJust mayConnModel $ \connModel -> do
            update $ action & Action.connectSnappedPort ?~ portRef
            Global.modifyNodeEditor $ NodeEditor.currentConnection ?= toCurrentConnection connModel

cancelSnapToPort :: AnyPortRef -> Connect -> Command State ()
cancelSnapToPort portRef action = when (Just portRef == action ^. Action.connectSnappedPort) $
    update $ action & Action.connectSnappedPort .~ Nothing

handleMouseUp :: MouseEvent -> Connect -> Command State ()
handleMouseUp evt action = when (action ^. Action.connectMode == Drag) $ do
    mousePos <- mousePosition evt
    if (mousePos == action ^. Action.connectStartPos) then
        update $ action & Action.connectMode .~ Click
    else stopConnecting action

stopConnecting :: Connect -> Command State ()
stopConnecting _ = do
    Global.modifyNodeEditor $ NodeEditor.currentConnection .= Nothing
    showOrHideAllSelfPorts Nothing Nothing
    removeActionFromState connectAction

connectToPort :: AnyPortRef -> Connect -> Command State ()
connectToPort dst action = do
    withJust (toValidConnection dst $ action ^. Action.connectSourcePort) $ \newConn -> do
        connectNodes (newConn ^. Connection.src) (newConn ^. Connection.dst)
        GA.sendEvent $ GA.Connect GA.Manual
    stopConnecting action

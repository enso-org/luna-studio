{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeApplications #-}
module Luna.Studio.Action.Connect
    ( handlePortMouseDown
    , handleConnectionMouseDown
    , handleClick
    , handleMove
    , handleMouseUp
    , stopConnecting
    , handlePortMouseUp
    , snapToPort
    , cancelSnapToPort
    ) where

import qualified Data.HashMap.Strict                    as HashMap
import           Empire.API.Data.Connection             (ConnectionId, toValidConnection)
import qualified Empire.API.Data.Connection             as Connection
import           Empire.API.Data.Port                   (InPort (Self), PortId (InPortId))
import           Empire.API.Data.PortRef                (AnyPortRef (InPortRef', OutPortRef'))
import qualified Empire.API.Data.PortRef                as PortRef
import qualified JS.GoogleAnalytics                     as GA
import           Luna.Studio.Action.Command             (Command)
import           Luna.Studio.Action.Geometry.Connection (createConnectionModel, createCurrentConnectionModel)
import           Luna.Studio.Action.Graph.Connect       (connectNodes)
import           Luna.Studio.Action.Graph.Disconnect    (removeConnections)
import           Luna.Studio.Action.Node.Drag           (startNodeDrag)
import           Luna.Studio.Action.Port.Self           (removeIdleSelfPorts, showAllSelfPorts)
import           Luna.Studio.Event.Mouse                (workspacePosition)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Event.Connection     (ModifiedEnd (Destination, Source))
import           Luna.Studio.React.Model.Connection     (toCurrentConnection)
import qualified Luna.Studio.React.Model.Node           as Model
import qualified Luna.Studio.React.Model.NodeEditor     as NodeEditor
import           Luna.Studio.State.Action               (Action (begin, continue, update), Connect, ConnectMode (ClickConnect, DragConnect),
                                                         connectAction)
import qualified Luna.Studio.State.Action               as Action
import           Luna.Studio.State.Global               (State, beginActionWithKey, checkAction, continueActionWithKey,
                                                         removeActionFromState, updateActionWithKey)
import qualified Luna.Studio.State.Global               as Global
import qualified Luna.Studio.State.Graph                as Graph
import           React.Flux                             (MouseEvent)


instance Action (Command State) Connect where
    begin    = beginActionWithKey    connectAction
    continue = continueActionWithKey connectAction
    update   = updateActionWithKey   connectAction
    end      = stopConnecting

handlePortMouseDown :: MouseEvent -> AnyPortRef -> Command State ()
handlePortMouseDown evt anyPortRef = do
    mayAction <- checkAction connectAction
    when (Just ClickConnect /= (view Action.connectMode <$> mayAction)) $
        startConnecting evt anyPortRef Nothing DragConnect

handleConnectionMouseDown :: MouseEvent -> ConnectionId -> ModifiedEnd -> Command State ()
handleConnectionMouseDown evt connId modifiedEnd = do
    connectionsMap <- use $ Global.graph . Graph.connectionsMap
    withJust (HashMap.lookup connId connectionsMap) $ \connection -> do
        let portRef = case modifiedEnd of
                Destination -> OutPortRef' (connection ^. Connection.src)
                Source      -> InPortRef'  (connection ^. Connection.dst)
        startConnecting evt portRef (Just connId) DragConnect

handleClick :: MouseEvent -> AnyPortRef -> Command State ()
handleClick evt anyPortRef = do
    mayAction <- checkAction connectAction
    if (Just ClickConnect == (view Action.connectMode <$> mayAction)) then
        continue $ connectToPort anyPortRef
    else startConnecting evt anyPortRef Nothing ClickConnect

startConnecting :: MouseEvent -> AnyPortRef -> Maybe ConnectionId -> ConnectMode -> Command State ()
startConnecting evt anyPortRef mayModifiedConnId connectMode = do
    let nodeId = anyPortRef ^. PortRef.nodeId
        portId = anyPortRef ^. PortRef.portId
    mousePos <- workspacePosition evt
    mayNode <- Global.getNode nodeId
    withJust mayNode $ \node -> do
        case (portId, node ^. Model.isExpanded) of
            (InPortId Self, False) -> when (connectMode == DragConnect) $ startNodeDrag evt nodeId True
            _                      -> do
                mayCurrentConnectionModel <- createCurrentConnectionModel anyPortRef mousePos
                when (isJust mayCurrentConnectionModel) $ do
                    withJust mayModifiedConnId $ removeConnections . replicate 1
                    begin $ Action.Connect mousePos anyPortRef Nothing connectMode
                    Global.modifyNodeEditor $ do
                        withJust mayModifiedConnId $ \connId ->
                            NodeEditor.connections . at connId .= Nothing
                        NodeEditor.currentConnection .= mayCurrentConnectionModel
                    case anyPortRef of
                        OutPortRef' _ -> showAllSelfPorts
                        _             -> return ()

handleMove :: MouseEvent -> Connect -> Command State ()
handleMove evt action = when (isNothing $ action ^. Action.connectSnappedPort) $ do
    mousePos                  <- workspacePosition evt
    mayCurrentConnectionModel <- createCurrentConnectionModel (action ^. Action.connectSourcePort) mousePos
    Global.modifyNodeEditor $ NodeEditor.currentConnection .= mayCurrentConnectionModel
    when (isNothing mayCurrentConnectionModel) $ stopConnecting action

handlePortMouseUp :: AnyPortRef -> Connect -> Command State ()
handlePortMouseUp portRef action = when (action ^. Action.connectMode == DragConnect) $
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
handleMouseUp evt action = when (action ^. Action.connectMode == DragConnect) $ do
    mousePos <- workspacePosition evt
    if (mousePos == action ^. Action.connectStartPos) then
        update $ action & Action.connectMode .~ ClickConnect
    else stopConnecting action

stopConnecting :: Connect -> Command State ()
stopConnecting _ = do
    Global.modifyNodeEditor $ NodeEditor.currentConnection .= Nothing
    removeActionFromState connectAction
    removeIdleSelfPorts Nothing

connectToPort :: AnyPortRef -> Connect -> Command State ()
connectToPort dst action = do
    withJust (toValidConnection dst $ action ^. Action.connectSourcePort) $ \newConn -> do
        connectNodes (newConn ^. Connection.src) (newConn ^. Connection.dst)
        GA.sendEvent $ GA.Connect GA.Manual
    stopConnecting action

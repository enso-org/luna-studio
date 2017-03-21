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

import qualified Data.HashMap.Strict                 as HashMap
import           Data.ScreenPosition                 (ScreenPosition)
import           Empire.API.Data.Connection          (ConnectionId)
import qualified Empire.API.Data.Connection          as Connection
import           Empire.API.Data.Port                (InPort (Self), PortId (InPortId))
import           Empire.API.Data.PortRef             (AnyPortRef (InPortRef', OutPortRef'))
import qualified Empire.API.Data.PortRef             as PortRef
import qualified JS.GoogleAnalytics                  as GA
import           Luna.Studio.Action.Basic            (connect, removeConnection, updateAllPortsSelfVisibility)
import           Luna.Studio.Action.Command          (Command)
import           Luna.Studio.Action.Node.Drag        (startNodeDrag)
import           Luna.Studio.Action.State.Action     (beginActionWithKey, continueActionWithKey, removeActionFromState, updateActionWithKey)
import           Luna.Studio.Action.State.Model      (createConnectionModel, createCurrentConnectionModel)
import           Luna.Studio.Action.State.NodeEditor (getNode, modifyNodeEditor)
import           Luna.Studio.Action.State.Scene      (translateToWorkspace)
import           Luna.Studio.Event.Mouse             (mousePosition, workspacePosition)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Event.Connection  (ModifiedEnd (Destination, Source))
import           Luna.Studio.React.Model.Connection  (toCurrentConnection)
import           Luna.Studio.React.Model.Node        (isCollapsed)
import qualified Luna.Studio.React.Model.NodeEditor  as NodeEditor
import           Luna.Studio.State.Action            (Action (begin, continue, end, update), Connect (Connect), Mode (Click, Drag),
                                                      connectAction, connectMode, connectSnappedPort, connectSourcePort, connectStartPos)
import           Luna.Studio.State.Global            (State, currentConnectAction)
import qualified Luna.Studio.State.Global            as Global
import qualified Luna.Studio.State.Graph             as Graph
import           React.Flux                          (MouseEvent)


instance Action (Command State) Connect where
    begin action = beginActionWithKey    connectAction action >> currentConnectAction ?= action
    continue     = continueActionWithKey connectAction
    update       = updateActionWithKey   connectAction
    end          = stopConnecting


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
startConnecting screenMousePos anyPortRef mayModifiedConnId connectMode' = do
    let nodeId = anyPortRef ^. PortRef.nodeId
        portId = anyPortRef ^. PortRef.portId
    mousePos <- translateToWorkspace screenMousePos
    mayNode  <- getNode nodeId
    withJust mayNode $ \node -> do
        case (portId, not . isCollapsed $ node) of
            (InPortId Self, False) -> when (connectMode' == Drag) $ startNodeDrag mousePos nodeId True
            _                      -> do
                mayCurrentConnectionModel <- createCurrentConnectionModel anyPortRef mousePos
                when (isJust mayCurrentConnectionModel) $ do
                    let action = Connect screenMousePos anyPortRef (isJust mayModifiedConnId) Nothing connectMode'
                    withJust mayModifiedConnId removeConnection
                    begin action
                    void $ updateAllPortsSelfVisibility
                    modifyNodeEditor $ do
                        withJust mayModifiedConnId $ \connId ->
                            NodeEditor.connections . at connId .= Nothing
                        NodeEditor.currentConnections .= maybeToList mayCurrentConnectionModel

handleMove :: MouseEvent -> Connect -> Command State ()
handleMove evt action = when (isNothing $ action ^. connectSnappedPort) $ do
    mousePos                  <- workspacePosition evt
    mayCurrentConnectionModel <- createCurrentConnectionModel (action ^. connectSourcePort) mousePos
    modifyNodeEditor $ NodeEditor.currentConnections .= maybeToList mayCurrentConnectionModel
    when (isNothing mayCurrentConnectionModel) $ stopConnecting action

handlePortMouseUp :: AnyPortRef -> Connect -> Command State ()
handlePortMouseUp portRef action = when (action ^. connectMode == Drag) $
    connectToPort portRef action

snapToPort :: AnyPortRef -> Connect -> Command State ()
snapToPort portRef action = undefined
    -- withJust (toValidConnection (action ^. connectSourcePort) portRef) $ \conn -> do
    --     mayConnModel <- createConnectionModel conn
    --     withJust mayConnModel $ \connModel -> do
    --         update $ action & connectSnappedPort ?~ portRef
    --         modifyNodeEditor $ NodeEditor.currentConnections .= [toCurrentConnection connModel]

cancelSnapToPort :: AnyPortRef -> Connect -> Command State ()
cancelSnapToPort portRef action = when (Just portRef == action ^. connectSnappedPort) $
    update $ action & connectSnappedPort .~ Nothing

handleMouseUp :: MouseEvent -> Connect -> Command State ()
handleMouseUp evt action = when (action ^. connectMode == Drag) $ do
    mousePos <- mousePosition evt
    if (mousePos == action ^. connectStartPos) then
        update $ action & connectMode .~ Click
    else stopConnecting action

stopConnecting :: Connect -> Command State ()
stopConnecting _ = do
    modifyNodeEditor $ NodeEditor.currentConnections .= def
    void $ updateAllPortsSelfVisibility
    currentConnectAction .= Nothing
    removeActionFromState connectAction

connectToPort :: AnyPortRef -> Connect -> Command State ()
connectToPort dst action = undefined
  -- do
  --   withJust (toValidConnection dst $ action ^. connectSourcePort) $ \newConn -> do
  --       connect (Left $ newConn ^. Connection.src) (Left $ newConn ^. Connection.dst)
  --       GA.sendEvent $ GA.Connect GA.Manual
  --   stopConnecting action

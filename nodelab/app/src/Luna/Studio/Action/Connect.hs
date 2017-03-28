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

import           Data.ScreenPosition                         (ScreenPosition)
import qualified Empire.API.Data.Connection                  as ConnectionAPI
import           Empire.API.Data.Port                        (InPort (Self), PortId (InPortId))
import qualified JS.GoogleAnalytics                          as GA
import           Luna.Studio.Action.Basic                    (connect, removeConnection, updateAllPortsSelfVisibility)
import           Luna.Studio.Action.Command                  (Command)
import           Luna.Studio.Action.Node.Drag                (startNodeDrag)
import           Luna.Studio.Action.State.Action             (beginActionWithKey, continueActionWithKey, removeActionFromState,
                                                              updateActionWithKey)
import           Luna.Studio.Action.State.Model              (createConnectionModel, createCurrentConnectionModel)
import           Luna.Studio.Action.State.NodeEditor         (getConnection, getNode, modifyNodeEditor)
import           Luna.Studio.Action.State.Scene              (translateToWorkspace)
import           Luna.Studio.Data.PortRef                    (AnyPortRef (InPortRef', OutPortRef'))
import qualified Luna.Studio.Data.PortRef                    as PortRef
import           Luna.Studio.Event.Mouse                     (mousePosition, workspacePosition)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Event.Connection          (ModifiedEnd (Destination, Source))
import           Luna.Studio.React.Model.Connection          (ConnectionId, toValidEmpireConnection)
import qualified Luna.Studio.React.Model.Connection          as Connection
import           Luna.Studio.React.Model.Node                (Node (Expression))
import           Luna.Studio.React.Model.Node.ExpressionNode (isCollapsed)
import qualified Luna.Studio.React.Model.NodeEditor          as NodeEditor
import           Luna.Studio.State.Action                    (Action (begin, continue, end, update), Connect (Connect), Mode (Click, Drag),
                                                              connectAction, connectMode, connectSnappedPort, connectSourcePort,
                                                              connectStartPos)
import           Luna.Studio.State.Global                    (State, actions, currentConnectAction)
import           React.Flux                                  (MouseEvent)


instance Action (Command State) Connect where
    begin action = beginActionWithKey    connectAction action >> actions . currentConnectAction ?= action
    continue     = continueActionWithKey connectAction
    update       = updateActionWithKey   connectAction
    end          = stopConnecting


handleConnectionMouseDown :: MouseEvent -> ConnectionId -> ModifiedEnd -> Command State ()
handleConnectionMouseDown evt connId modifiedEnd = do
    withJustM (getConnection connId) $ \connection -> do
        let portRef = case modifiedEnd of
                Destination -> OutPortRef' (connection ^. Connection.src)
                Source      -> InPortRef'  (connection ^. Connection.dst)
        mousePos <- mousePosition evt
        startConnecting mousePos portRef (Just connId) Drag

startConnecting :: ScreenPosition -> AnyPortRef -> Maybe ConnectionId -> Mode -> Command State ()
startConnecting screenMousePos anyPortRef mayModifiedConnId connectMode' = do
    let nodeLoc = anyPortRef ^. PortRef.nodeLoc
        portId  = anyPortRef ^. PortRef.portId
    mousePos <- translateToWorkspace screenMousePos
    mayNode  <- getNode nodeLoc
    withJust mayNode $ \node -> do
        let shouldDoNodeDrag = case node of
                Expression node' -> isNothing mayModifiedConnId
                                 && portId == InPortId Self
                                 && isCollapsed node'
                _                -> False
        if shouldDoNodeDrag
        then when (connectMode' == Drag) $ startNodeDrag mousePos nodeLoc True
        else do
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
snapToPort portRef action =
    withJust (toValidEmpireConnection (action ^. connectSourcePort) portRef) $ \conn -> do
        mayConnModel <- createConnectionModel (convert $ conn ^. ConnectionAPI.src) (convert $ conn ^. ConnectionAPI.dst)
        withJust mayConnModel $ \connModel -> do
            update $ action & connectSnappedPort ?~ portRef
            modifyNodeEditor $ NodeEditor.currentConnections .= [convert connModel]

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
    actions . currentConnectAction .= Nothing
    removeActionFromState connectAction
    void $ updateAllPortsSelfVisibility

connectToPort :: AnyPortRef -> Connect -> Command State ()
connectToPort dst action = do
    withJust (toValidEmpireConnection dst $ action ^. connectSourcePort) $ \newConn -> do
        connect (Left $ convert $ newConn ^. ConnectionAPI.src) (Left $ convert $ newConn ^. ConnectionAPI.dst)
        GA.sendEvent $ GA.Connect GA.Manual
    stopConnecting action

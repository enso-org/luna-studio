{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeApplications #-}
module NodeEditor.Action.Connect
    ( handleConnectionMouseDown
    , handleMove
    , handleMouseUp
    , handlePortMouseUp
    , startConnecting
    , snapToPort
    , cancelSnapToPort
    , connectToPort
    , stopConnectingUnsafe
    ) where

import           Common.Action.Command                      (Command)
import           Common.Prelude
import           Control.Monad.Trans.Maybe                  (MaybeT (MaybeT), runMaybeT)
import qualified LunaStudio.Data.Connection                 as ConnectionAPI
import           LunaStudio.Data.PortRef                    (AnyPortRef (InPortRef', OutPortRef'))
import qualified LunaStudio.Data.PortRef                    as PortRef
import           LunaStudio.Data.ScreenPosition             (ScreenPosition)
import           LunaStudio.Data.Position                   (Position)
import           NodeEditor.Action.Basic                    (connect, localAddConnection, localRemovePort, removeConnection,
                                                             updateAllPortsMode)
import qualified NodeEditor.Action.Batch                    as Batch
import           NodeEditor.Action.NodeDrag                 (startNodeDrag)
import           NodeEditor.Action.State.Action             (beginActionWithKey, continueActionWithKey, removeActionFromState,
                                                             updateActionWithKey)
import           NodeEditor.Action.State.Model              (createHalfConnectionModel, createHalfConnectionModel')
import           NodeEditor.Action.State.NodeEditor         (getConnection, getNode, inTopLevelBreadcrumb, modifyNodeEditor)
import           NodeEditor.Action.State.ResetNode          (resetSuccessors)
import           NodeEditor.Action.State.Scene              (translateToWorkspace)
import           NodeEditor.React.Event.Connection          (ModifiedEnd (Destination, Source))
import           NodeEditor.React.Model.Connection          (ConnectionId, toValidConnection)
import qualified NodeEditor.React.Model.Connection          as Connection
import           NodeEditor.React.Model.Node                (Node (Expression))
import           NodeEditor.React.Model.Node.ExpressionNode (isCollapsed)
import qualified NodeEditor.React.Model.NodeEditor          as NodeEditor
import qualified NodeEditor.React.Model.Port                as Port
import           NodeEditor.State.Action                    (Action (begin, continue, end, update), Connect (Connect), Mode (Click, Drag),
                                                             connectAction, connectIsArgumentConstructor, connectMode, connectSnappedPort,
                                                             connectSourcePort, connectStartPos)
import           NodeEditor.State.Global                    (State, actions, currentConnectAction)


instance Action (Command State) Connect where
    begin action = do
        beginActionWithKey connectAction action
        actions . currentConnectAction ?= action
        updateAllPortsMode
    continue     = continueActionWithKey connectAction
    update       = updateActionWithKey   connectAction
    end action   = do
        stopConnectingUnsafe action
        when (action ^. connectIsArgumentConstructor)
            $ case action ^. connectSourcePort of
                OutPortRef' outPortRef -> void $ localRemovePort outPortRef
                _                      -> return ()

handleConnectionMouseDown :: ScreenPosition -> ConnectionId -> ModifiedEnd
    -> Command State ()
handleConnectionMouseDown mousePos connId modifiedEnd = do
    withJustM (getConnection connId) $ \connection -> do
        let portRef = case modifiedEnd of
                Destination -> OutPortRef' (connection ^. Connection.src)
                Source      -> InPortRef'  (connection ^. Connection.dst)
        startConnecting mousePos portRef (Just connId) False Drag

startConnecting :: ScreenPosition -> AnyPortRef -> Maybe ConnectionId -> Bool
    -> Mode -> Command State ()
startConnecting screenMousePos anyPortRef mayModifiedConnId
    isArgumentConstructor connectMode' = unlessM inTopLevelBreadcrumb $ do
        let nodeLoc = anyPortRef ^. PortRef.nodeLoc
            portId  = anyPortRef ^. PortRef.portId
        mousePos <- translateToWorkspace screenMousePos
        maySuccess <- runMaybeT $ do
            node <- MaybeT $ getNode nodeLoc
            let shouldDoNodeDrag = case node of
                    Expression node' -> isNothing mayModifiedConnId
                                     && Port.isSelf portId
                                     && isCollapsed node'
                    _                -> False
            if shouldDoNodeDrag
                then lift $ when (connectMode' == Drag)
                    $ startNodeDrag mousePos nodeLoc True
                else do
                    halfConnectionModel <- MaybeT
                        $ createHalfConnectionModel anyPortRef mousePos
                    let action = Connect
                            screenMousePos
                            anyPortRef
                            (isJust mayModifiedConnId)
                            Nothing
                            isArgumentConstructor
                            connectMode'
                    lift $ do
                        withJust mayModifiedConnId removeConnection
                        begin action
                        modifyNodeEditor $ do
                            withJust mayModifiedConnId $ \connId ->
                                NodeEditor.connections . at connId .= Nothing
                            NodeEditor.halfConnections .= [halfConnectionModel]
        when (isNothing maySuccess && isArgumentConstructor)
            $ case anyPortRef of
                OutPortRef' outPortRef -> void $ localRemovePort outPortRef
                _                      -> return ()

handleMove :: Position -> Connect -> Command State ()
handleMove workspacePos action = when (isNothing $ action ^. connectSnappedPort) $ do
    mayHalfConnectionModel <-
        createHalfConnectionModel (action ^. connectSourcePort) workspacePos
    modifyNodeEditor
        $ NodeEditor.halfConnections .= maybeToList mayHalfConnectionModel
    when (isNothing mayHalfConnectionModel) $ end action

handlePortMouseUp :: AnyPortRef -> Connect -> Command State ()
handlePortMouseUp portRef action = when (action ^. connectMode == Drag) $
    connectToPort portRef action

snapToPort :: AnyPortRef -> Connect -> Command State ()
snapToPort portRef action =
    withJust (toValidConnection (action ^. connectSourcePort) portRef)
        $ \conn -> do
            mayConnModel <- createHalfConnectionModel'
                (conn ^. ConnectionAPI.src)
                (conn ^. ConnectionAPI.dst)
            withJust mayConnModel $ \connModel -> do
                update $ action & connectSnappedPort ?~ portRef
                modifyNodeEditor $ NodeEditor.halfConnections .= [connModel]

cancelSnapToPort :: AnyPortRef -> Connect -> Command State ()
cancelSnapToPort portRef action
    = when (Just portRef == action ^. connectSnappedPort) $
        update $ action & connectSnappedPort .~ Nothing

handleMouseUp :: ScreenPosition -> Connect -> Command State ()
handleMouseUp mousePos action = when (action ^. connectMode == Drag) $ do
    if (mousePos == action ^. connectStartPos) then
        update $ action & connectMode .~ Click
    else end action

stopConnectingUnsafe :: Connect -> Command State ()
stopConnectingUnsafe _ = do
    modifyNodeEditor $ NodeEditor.halfConnections .= def
    actions . currentConnectAction .= Nothing
    removeActionFromState connectAction
    updateAllPortsMode

connectToPort :: AnyPortRef -> Connect -> Command State ()
connectToPort dst action = do
    withJust (toValidConnection dst $ action ^. connectSourcePort)
        $ \newConn -> case (action ^. connectIsArgumentConstructor
            , action ^. connectSourcePort) of
                (True, OutPortRef' outPortRef) -> do
                    void . localAddConnection
                        . ConnectionAPI.Connection outPortRef
                            $ newConn ^. ConnectionAPI.dst
                    resetSuccessors $ dst ^. PortRef.nodeLoc
                    Batch.addPort
                        outPortRef
                        (Just $ newConn ^. ConnectionAPI.dst)
                        def
                _ -> connect
                    (Left $ newConn ^. ConnectionAPI.src)
                    (Left $ newConn ^. ConnectionAPI.dst)
    stopConnectingUnsafe action

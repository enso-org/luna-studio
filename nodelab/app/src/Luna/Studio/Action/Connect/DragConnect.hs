{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeApplications #-}
module Luna.Studio.Action.Connect.DragConnect
    ( startDragConnect
    , dragModifyConnection
    , handleDragConnectMouseUp
    , dragConnectToPort
    ) where

import           Empire.API.Data.Connection              (ConnectionId)
import           Empire.API.Data.PortRef                 (AnyPortRef)
import           Luna.Studio.Action.Command              (Command)
import           Luna.Studio.Action.Connect.ClickConnect ()
import           Luna.Studio.Action.Connect.Connect      (connectToPort, modifyConnection, startOrModifyConnection, stopConnecting,
                                                          whileConnecting)
import           Luna.Studio.Event.Mouse                 (mousePosition)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Event.Connection      (ModifiedEnd)
import           Luna.Studio.State.Action                (Action (begin, continue, end, update), ClickConnect (ClickConnect),
                                                          DragConnect (DragConnect), clickConnectAction, dragConnectAction)
import qualified Luna.Studio.State.Action                as Action
import           Luna.Studio.State.Global                (State, beginActionWithKey, checkAction, continueActionWithKey,
                                                          removeActionFromState, updateActionWithKey)
import           React.Flux                              (MouseEvent)


instance Action (Command State) DragConnect where
    begin    = beginActionWithKey    dragConnectAction
    continue = continueActionWithKey dragConnectAction
    update   = updateActionWithKey   dragConnectAction
    end      = stopDragConnect

startDragConnect :: MouseEvent -> AnyPortRef -> Command State ()
startDragConnect evt portRef = whenM (isNothing <$> checkAction @ClickConnect) $ do
    mousePos <- mousePosition evt
    begin (DragConnect mousePos) >> startOrModifyConnection evt portRef

dragModifyConnection :: MouseEvent -> ConnectionId -> ModifiedEnd -> Command State ()
dragModifyConnection evt connId modifiedEnd = do
    mousePos <- mousePosition evt
    begin (DragConnect mousePos)
    modifyConnection evt connId modifiedEnd

dragConnectToPort :: AnyPortRef -> DragConnect -> Command State ()
dragConnectToPort portRef _ = do
    removeActionFromState dragConnectAction
    whileConnecting $ connectToPort portRef

handleDragConnectMouseUp :: MouseEvent -> DragConnect -> Command State ()
handleDragConnectMouseUp evt state = do
    mousePos <- mousePosition evt
    if (mousePos == state ^. Action.dragConnectStartPos) then do
        removeActionFromState dragConnectAction
        updateActionWithKey clickConnectAction ClickConnect
    else stopDragConnect state

stopDragConnect :: DragConnect -> Command State ()
stopDragConnect _ = do
    removeActionFromState dragConnectAction
    whileConnecting $ stopConnecting

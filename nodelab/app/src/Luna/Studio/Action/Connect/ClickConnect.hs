{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeApplications #-}
module Luna.Studio.Action.Connect.ClickConnect
    ( handleClickConnect
    , stopClickConnect
    , clickConnectToPort
    ) where

import           Empire.API.Data.PortRef            (AnyPortRef)
import           Luna.Studio.Action.Command         (Command)
import           Luna.Studio.Action.Connect.Connect (connectToPort, startOrModifyConnection, stopConnecting, whileConnecting)
import           Luna.Studio.Prelude
import           Luna.Studio.State.Action           (Action (begin, continue, end, update), ClickConnect (ClickConnect), clickConnectAction)
import           Luna.Studio.State.Global           (State, beginActionWithKey, checkAction, continueActionWithKey, removeActionFromState,
                                                     updateActionWithKey)
import           React.Flux                         (MouseEvent)


instance Action (Command State) ClickConnect where
    begin    = beginActionWithKey    clickConnectAction
    continue = continueActionWithKey clickConnectAction
    update   = updateActionWithKey   clickConnectAction
    end      = stopClickConnect

handleClickConnect :: MouseEvent -> AnyPortRef -> Command State ()
handleClickConnect evt portRef = do
    mayClickConnect <- checkAction @ClickConnect
    if (isJust mayClickConnect) then
        continue $ clickConnectToPort portRef
    else begin ClickConnect >> startOrModifyConnection evt portRef

clickConnectToPort :: AnyPortRef -> ClickConnect -> Command State ()
clickConnectToPort portRef _ = do
    removeActionFromState clickConnectAction
    whileConnecting $ connectToPort portRef

stopClickConnect :: ClickConnect -> Command State ()
stopClickConnect _ = do
    removeActionFromState clickConnectAction
    whileConnecting $ stopConnecting

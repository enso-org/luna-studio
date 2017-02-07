{-# LANGUAGE TypeApplications #-}
module Luna.Studio.Action.Port.Highlight
    ( setHighlight
    ) where

import qualified Data.Set                     as Set
import           Empire.API.Data.Port         (InPort (Self), PortId (InPortId))
import           Empire.API.Data.PortRef      (AnyPortRef)
import qualified Empire.API.Data.PortRef      as PortRef
import           Luna.Studio.Action.Command   (Command)
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Model.Node as NodeModel
import qualified Luna.Studio.React.Model.Port as PortModel
import           Luna.Studio.State.Action     (actionsBlockingPortHighlight)
import           Luna.Studio.State.Action     (clickConnectAction, dragConnectAction)
import           Luna.Studio.State.Global     (State, getNode, runningActions)
import qualified Luna.Studio.State.Global     as Global

setHighlight :: AnyPortRef -> Bool -> Command State ()
setHighlight portRef highlight = do
    let nodeId  = portRef ^. PortRef.nodeId
    mayNode <- getNode nodeId
    withJust mayNode $ \node -> do
        actions <- Set.fromList <$> runningActions
        let notBlocked = Set.null (Set.intersection actions actionsBlockingPortHighlight)
            shouldProceed = highlight == False
                         || ( notBlocked
                            && ( portRef ^. PortRef.portId /= InPortId Self
                              || node ^. NodeModel.isExpanded
                              || elem clickConnectAction actions
                              || elem dragConnectAction  actions ))
        print highlight
        print notBlocked
        print $ portRef ^. PortRef.portId /= InPortId Self
        print $ node ^. NodeModel.isExpanded
        print $ elem clickConnectAction actions
        print $ elem dragConnectAction actions
        print shouldProceed

        when shouldProceed $ Global.modifyNode nodeId $
            NodeModel.ports . ix portRef . PortModel.highlight .= highlight

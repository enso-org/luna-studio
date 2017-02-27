{-# LANGUAGE TypeApplications #-}
module Luna.Studio.Action.Port.Highlight
    ( handleMouseEnter
    , handleMouseLeave
    ) where

import qualified Data.Set                     as Set
import           Empire.API.Data.Connection   (toValidConnection)
import           Empire.API.Data.Port         (InPort (Self), PortId (InPortId))
import           Empire.API.Data.PortRef      (AnyPortRef)
import qualified Empire.API.Data.PortRef      as PortRef
import           Luna.Studio.Action.Command   (Command)
import           Luna.Studio.Action.Connect   ()
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Model.Node as NodeModel
import qualified Luna.Studio.React.Model.Port as PortModel
import           Luna.Studio.State.Action     (actionsBlockingPortHighlight, connectAction)
import qualified Luna.Studio.State.Action     as Action
import           Luna.Studio.State.Global     (State, checkAction, getNode, runningActions)
import qualified Luna.Studio.State.Global     as Global


handleMouseEnter :: AnyPortRef -> Command State ()
handleMouseEnter portRef = do
    let nodeId = portRef ^. PortRef.nodeId
    let portId = portRef ^. PortRef.portId
    mayNode <- getNode nodeId
    withJust mayNode $ \node -> do
        mayConnectAction <- checkAction connectAction
        case (view Action.connectSourcePort <$> mayConnectAction) of
            Just src -> when (isJust $ toValidConnection src portRef) $ Global.modifyNode nodeId $
                NodeModel.ports . ix portId . PortModel.highlight .= True
            Nothing  -> do
                actions <- Set.fromList <$> runningActions
                let notBlocked = Set.null (Set.intersection actions actionsBlockingPortHighlight)
                    highlight  = notBlocked && (portId /= InPortId Self || not (node ^. NodeModel.isCollapsed))
                Global.modifyNode nodeId $
                    NodeModel.ports . ix portId . PortModel.highlight .= highlight

handleMouseLeave :: AnyPortRef -> Command State ()
handleMouseLeave portRef = Global.modifyNode (portRef ^. PortRef.nodeId) $
    NodeModel.ports. ix (portRef ^. PortRef.portId) . PortModel.highlight .= False

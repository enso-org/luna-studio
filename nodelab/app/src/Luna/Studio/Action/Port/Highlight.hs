{-# LANGUAGE TypeApplications #-}
module Luna.Studio.Action.Port.Highlight
    ( handleMouseEnter
    , handleMouseLeave
    ) where

import qualified Data.Set                            as Set
import           Empire.API.Data.Connection          (toValidConnection)
import           Empire.API.Data.Port                (InPort (Self), PortId (InPortId))
import           Empire.API.Data.PortRef             (AnyPortRef, nodeId, portId)
import           Luna.Studio.Action.Command          (Command)
import           Luna.Studio.Action.Connect          ()
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Node        (isCollapsed, ports)
import           Luna.Studio.React.Model.Port        (highlight)
import           Luna.Studio.State.Action            (actionsBlockingPortHighlight, connectAction, connectSourcePort)

import           Luna.Studio.Action.State.Action     (checkAction, runningActions)
import           Luna.Studio.Action.State.NodeEditor (getNode, modifyNode)
import           Luna.Studio.State.Global            (State)


handleMouseEnter :: AnyPortRef -> Command State ()
handleMouseEnter portRef = do
    let nid = portRef ^. nodeId
    let pid = portRef ^. portId
    mayNode <- getNode nid
    withJust mayNode $ \node -> do
        mayConnectAction <- checkAction connectAction
        case (view connectSourcePort <$> mayConnectAction) of
            Just src -> when (isJust $ toValidConnection src portRef) $
                modifyNode nid $ ports . ix pid . highlight .= True
            Nothing  -> do
                actions <- Set.fromList <$> runningActions
                let notBlocked = Set.null (Set.intersection actions actionsBlockingPortHighlight)
                    highlight' = notBlocked && (pid /= InPortId Self || (not . isCollapsed $ node))
                modifyNode nid $ ports . ix pid . highlight .= highlight'

handleMouseLeave :: AnyPortRef -> Command State ()
handleMouseLeave portRef = modifyNode (portRef ^. nodeId) $
    ports. ix (portRef ^. portId) . highlight .= False

{-# LANGUAGE TypeApplications #-}
module Luna.Studio.Action.Port.Highlight
    ( handleMouseEnter
    , handleMouseLeave
    ) where

import qualified Data.Set                                    as Set
import           Empire.API.Data.Port                        (InPort (Self), PortId (InPortId))
import           Empire.API.Data.PortRef                     (AnyPortRef, nodeId, portId)
import           Luna.Studio.Action.Command                  (Command)
import           Luna.Studio.Action.Connect                  ()
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Connection          (toValidEmpireConnection)
import           Luna.Studio.React.Model.Node.ExpressionNode (isCollapsed, ports)
import           Luna.Studio.React.Model.Port                (highlight)
import           Luna.Studio.State.Action                    (actionsBlockingPortHighlight, connectAction, connectSourcePort)

import           Luna.Studio.Action.State.Action             (checkAction, runningActions)
import           Luna.Studio.Action.State.NodeEditor         (getExpressionNode, modifyExpressionNode)
import           Luna.Studio.State.Global                    (State)


handleMouseEnter :: AnyPortRef -> Command State ()
handleMouseEnter portRef = do
    let nid = portRef ^. nodeId
    let pid = portRef ^. portId
    mayNode <- getExpressionNode nid
    withJust mayNode $ \node -> do
        mayConnectAction <- checkAction connectAction
        case (view connectSourcePort <$> mayConnectAction) of
            Just src -> when (isJust $ toValidEmpireConnection src portRef) $
                modifyExpressionNode nid $ ports . ix pid . highlight .= True
            Nothing  -> do
                actions <- Set.fromList <$> runningActions
                let notBlocked = Set.null (Set.intersection actions actionsBlockingPortHighlight)
                    highlight' = notBlocked && (pid /= InPortId Self || (not . isCollapsed $ node))
                modifyExpressionNode nid $ ports . ix pid . highlight .= highlight'

handleMouseLeave :: AnyPortRef -> Command State ()
handleMouseLeave portRef = modifyExpressionNode (portRef ^. nodeId) $
    ports. ix (portRef ^. portId) . highlight .= False

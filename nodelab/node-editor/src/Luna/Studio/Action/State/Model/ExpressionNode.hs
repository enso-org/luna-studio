module Luna.Studio.Action.State.Model.ExpressionNode where

import           Control.Monad                               (filterM)
import           Data.Position                               (Position)
import           Data.ScreenPosition                         (fromDoubles)
import           Empire.API.Data.PortRef                     (InPortRef (InPortRef), toAnyPortRef)
import           Luna.Studio.Action.Command                  (Command)
import           Luna.Studio.Action.State.Action             (checkIfActionPerfoming)
import           Luna.Studio.Action.State.NodeEditor         (getExpressionNodes, inGraph)
import           Luna.Studio.Action.State.Scene              (translateToWorkspace)
import           Luna.Studio.Data.Geometry                   (isPointInCircle, isPointInRectangle)
import           Luna.Prelude
import           Luna.Studio.React.Model.Connection          (toValidEmpireConnection)
import           Luna.Studio.React.Model.Constants           (nodeRadius)
import           Luna.Studio.React.Model.Node.ExpressionNode (ExpressionNode, NodeLoc, hasPort, isCollapsed, nodeId, nodeLoc, nodeLoc,
                                                              position, position, zPos)
import           Luna.Studio.React.Model.Port                (AnyPortId (InPortId'), InPortIndex (Self))
import           Luna.Studio.State.Action                    (connectSourcePort, penConnectAction)
import           Luna.Studio.State.Global                    (State, actions, currentConnectAction)


foreign import javascript safe "document.getElementById($1).getBoundingClientRect().left"   expandedNodeLeft   :: JSString -> IO Double
foreign import javascript safe "document.getElementById($1).getBoundingClientRect().top"    expandedNodeTop    :: JSString -> IO Double
foreign import javascript safe "document.getElementById($1).getBoundingClientRect().right"  expandedNodeRight  :: JSString -> IO Double
foreign import javascript safe "document.getElementById($1).getBoundingClientRect().bottom" expandedNodeBottom :: JSString -> IO Double


isPointInNode :: Position -> ExpressionNode -> Command State Bool
isPointInNode p node =
    if isCollapsed node
        then return $ isPointInCircle p (node ^. position, nodeRadius)
        else do
            let nid = node ^. nodeId
            left        <- liftIO $ expandedNodeLeft   $ fromString $ "node-" <> show nid
            right       <- liftIO $ expandedNodeRight  $ fromString $ "node-" <> show nid
            top         <- liftIO $ expandedNodeTop    $ fromString $ "node-" <> show nid
            bottom      <- liftIO $ expandedNodeBottom $ fromString $ "node-" <> show nid
            leftTop     <- translateToWorkspace $ fromDoubles left  top
            rightBottom <- translateToWorkspace $ fromDoubles right bottom
            return $ isPointInRectangle p (leftTop, rightBottom)

getNodeAtPosition :: Position -> Command State (Maybe NodeLoc)
getNodeAtPosition p = do
    nodes <- getExpressionNodes >>= filterM (isPointInNode p)
    if null nodes
        then return Nothing
        else return $ Just $ maximumBy (\node1 node2 -> compare (node1 ^. zPos) (node2 ^. zPos)) nodes ^. nodeLoc



shouldDisplayPortSelf :: ExpressionNode -> Command State Bool
shouldDisplayPortSelf node = do
    let selfId = InPortId' [Self]
    if not $ hasPort selfId node
        then return False
        else do
            let nl = node ^. nodeLoc
            penConnecting    <- checkIfActionPerfoming penConnectAction
            mayConnectAction <- use $ actions . currentConnectAction
            let connectToSelfPossible = isJust . join $
                    (toValidEmpireConnection (toAnyPortRef nl selfId) . view connectSourcePort) <$> mayConnectAction
                isSource = (view connectSourcePort <$> mayConnectAction) == Just (toAnyPortRef nl selfId)
            if (not . isCollapsed $ node) || penConnecting || connectToSelfPossible || isSource
                then return True
                else inGraph $ InPortRef nl [Self]
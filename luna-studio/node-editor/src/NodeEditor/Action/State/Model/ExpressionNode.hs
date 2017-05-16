module NodeEditor.Action.State.Model.ExpressionNode where

import           Common.Prelude
import           Control.Monad                              (filterM)
import           Data.ScreenPosition                        (fromDoubles)
import           LunaStudio.Data.Geometry                   (isPointInCircle, isPointInRectangle)
import           LunaStudio.Data.PortRef                    (InPortRef (InPortRef), toAnyPortRef)
import           LunaStudio.Data.Position                   (Position)
import           NodeEditor.Action.Command                  (Command)
import           NodeEditor.Action.State.Action             (checkIfActionPerfoming)
import           NodeEditor.Action.State.NodeEditor         (getExpressionNodes, inGraph)
import           NodeEditor.Action.State.Scene              (translateToWorkspace)
import           NodeEditor.React.Model.Connection          (toValidEmpireConnection)
import           NodeEditor.React.Model.Constants           (nodeRadius)
import           NodeEditor.React.Model.Node.ExpressionNode (ExpressionNode, NodeLoc, hasPort, isCollapsed, nodeId, nodeLoc, nodeLoc,
                                                             position, position, zPos)
import           NodeEditor.React.Model.Port                (AnyPortId (InPortId'), InPortIndex (Self))
import           NodeEditor.State.Action                    (connectSourcePort, penConnectAction)
import           NodeEditor.State.Global                    (State, actions, currentConnectAction)


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

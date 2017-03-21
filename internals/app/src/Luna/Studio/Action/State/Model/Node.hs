module Luna.Studio.Action.State.Model.Node where

import           Control.Monad                       (filterM)
import           Data.Position                       (Position, x, y)
import           Data.ScreenPosition                 (fromDoubles)
import           Empire.API.Data.Node                (NodeId)
import           Luna.Studio.Action.Command          (Command)
import           Luna.Studio.Action.State.Graph      (inGraph)
import           Luna.Studio.Action.State.NodeEditor (getNodes)
import           Luna.Studio.Action.State.Scene      (translateToWorkspace)
import           Luna.Studio.Data.Angle              (Angle)
import           Luna.Studio.Data.Geometry           (isPointInCircle, isPointInRectangle)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Constants   (nodeRadius)
import           Luna.Studio.React.Model.Node        (Node, hasPort, isCollapsed, nodeId, position, position, zPos)
import           Luna.Studio.State.Global            (State, currentConnectAction)

-- import           Empire.API.Data.Connection          (toValidConnection)
import           Empire.API.Data.Port                (InPort (Self), PortId (InPortId))
import           Empire.API.Data.PortRef             (InPortRef (InPortRef), toAnyPortRef)
import           Luna.Studio.Action.State.Action     (checkIfActionPerfoming)
import           Luna.Studio.State.Action            (connectSourcePort, penConnectAction)


-- foreign import javascript safe "document.getElementById($1).getBoundingClientRect().left"   expandedNodeLeft   :: JSString -> IO Double
expandedNodeLeft   :: JSString -> IO Double
expandedNodeLeft = undefined
-- foreign import javascript safe "document.getElementById($1).getBoundingClientRect().top"    expandedNodeTop    :: JSString -> IO Double
expandedNodeTop    :: JSString -> IO Double
expandedNodeTop = undefined
-- foreign import javascript safe "document.getElementById($1).getBoundingClientRect().right"  expandedNodeRight  :: JSString -> IO Double
expandedNodeRight  :: JSString -> IO Double
expandedNodeRight = undefined
-- foreign import javascript safe "document.getElementById($1).getBoundingClientRect().bottom" expandedNodeBottom :: JSString -> IO Double
expandedNodeBottom :: JSString -> IO Double
expandedNodeBottom = undefined

nodeToNodeAngle :: Position -> Position -> Angle
nodeToNodeAngle src dst =
    let srcX = src ^. x
        srcY = src ^. y
        dstX = dst ^. x
        dstY = dst ^. y
    in  if srcX < dstX
            then atan ((srcY - dstY) / (srcX - dstX))
            else atan ((srcY - dstY) / (srcX - dstX)) + pi

isPointInNode :: Position -> Node -> Command State Bool
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

getNodeAtPosition :: Position -> Command State (Maybe NodeId)
getNodeAtPosition p = do
    nodes <- getNodes >>= filterM (isPointInNode p)
    if null nodes
        then return Nothing
        else return $ Just $ maximumBy (\node1 node2 -> compare (node1 ^. zPos) (node2 ^. zPos)) nodes ^. nodeId



shouldDisplayPortSelf :: Node -> Command State Bool
shouldDisplayPortSelf node = undefined --do
    -- let selfId = InPortId Self
    -- if not $ hasPort selfId node
    --     then return False
    --     else do
    --         let nid = node ^. nodeId
    --         connectToSelfPossible <- fmap (isJust . join) $ (fmap . fmap)
    --             ((toValidConnection $ toAnyPortRef nid selfId) . view connectSourcePort) $ use currentConnectAction
    --         penConnecting <- checkIfActionPerfoming penConnectAction
    --         if (not . isCollapsed $ node) || penConnecting || connectToSelfPossible
    --             then return True
    --             else inGraph $ InPortRef nid Self

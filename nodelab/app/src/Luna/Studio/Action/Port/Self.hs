module Luna.Studio.Action.Port.Self
    ( removeSelfIfNeeded
    , addPortSelfIfExists
    , showAllSelfPorts
    , removeIdleSelfPorts
    ) where

import qualified Data.Map.Lazy                      as Map
import qualified Empire.API.Data.Node               as Node
import           Empire.API.Data.Port               (InPort (Self), PortId (InPortId))
import           Empire.API.Data.PortRef            (InPortRef (InPortRef), toAnyPortRef)
import           Luna.Studio.Action.Command         (Command)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Node       (Node)
import qualified Luna.Studio.React.Model.Node       as Model
import qualified Luna.Studio.React.Model.NodeEditor as NodeEditor
import           Luna.Studio.React.Model.Port       (fromPort)
import           Luna.Studio.State.Global           (State, getConnection)
import qualified Luna.Studio.State.Global           as Global
import qualified Luna.Studio.State.Graph            as Graph


removeSelfIfNeeded :: Node -> Command State Node
removeSelfIfNeeded node = do
    let nodeId = node ^. Model.nodeId
        connId = InPortRef nodeId Self
    mayConn <- getConnection connId
    if (not $ node ^. Model.isExpanded) && isNothing mayConn then
        return $ node & Model.ports %~ (Map.delete (toAnyPortRef nodeId (InPortId Self)))
    else
        return node

addPortSelfIfExists :: Node -> Command State Node
addPortSelfIfExists node = do
    let nodeId = node ^. Model.nodeId
        key    = toAnyPortRef nodeId (InPortId Self)
    mayApiNode <- use $ Global.graph . Graph.nodesMap . at nodeId
    case mayApiNode of
        Nothing      -> return node
        Just apiNode -> case Map.lookup (InPortId Self) (apiNode ^. Node.ports) of
            Nothing      -> return node
            Just apiPort -> return $ node & Model.ports %~ (Map.insert key port) where
                port = fromPort nodeId apiPort

showAllSelfPorts :: Command State ()
showAllSelfPorts = do
    nodesMap    <- view NodeEditor.nodes <$> Global.getNodeEditor
    newNodesMap <- mapM addPortSelfIfExists nodesMap
    Global.modifyNodeEditor $ NodeEditor.nodes .= newNodesMap

removeIdleSelfPorts :: Command State ()
removeIdleSelfPorts = do
    nodesMap    <- view NodeEditor.nodes <$> Global.getNodeEditor
    newNodesMap <- mapM removeSelfIfNeeded nodesMap
    Global.modifyNodeEditor $ NodeEditor.nodes .= newNodesMap

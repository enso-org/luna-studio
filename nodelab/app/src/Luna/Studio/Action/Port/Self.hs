module Luna.Studio.Action.Port.Self
    ( showOrHideSelfPort
    , showOrHideAllSelfPorts
    ) where

import           Empire.API.Data.Connection         (toValidConnection)
import           Empire.API.Data.Port               (InPort (Self), PortId (InPortId))
import           Empire.API.Data.PortRef            (InPortRef (InPortRef), toAnyPortRef)
import           Luna.Studio.Action.Command         (Command)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Node       (Node)
import qualified Luna.Studio.React.Model.Node       as Model
import qualified Luna.Studio.React.Model.NodeEditor as NodeEditor
import qualified Luna.Studio.React.Model.Port       as Model
import           Luna.Studio.State.Action           (Connect, PenConnect)
import qualified Luna.Studio.State.Action           as Action
import           Luna.Studio.State.Global           (State, getConnection)
import qualified Luna.Studio.State.Global           as Global


showOrHideSelfPort :: Maybe Connect -> Maybe PenConnect -> Node -> Command State Node
showOrHideSelfPort mayConnect mayPenConnect node = do
    let nodeId                = node ^. Model.nodeId
        selfPortRef           = toAnyPortRef nodeId (InPortId Self)
        connectToSelfPossible = isJust $ join $
            mapM (toValidConnection selfPortRef) $ view Action.connectSourcePort <$> mayConnect
    isConnected <- isJust <$> getConnection (InPortRef nodeId Self)
    if ( isJust mayPenConnect
      || connectToSelfPossible
      || node ^. Model.isExpanded
      || isConnected ) then
         return $ node & Model.ports . at selfPortRef . _Just . Model.visible .~ True
    else return $ node & Model.ports . at selfPortRef . _Just . Model.visible .~ False

showOrHideAllSelfPorts :: Maybe Connect -> Maybe PenConnect -> Command State ()
showOrHideAllSelfPorts mayConnect mayPenConnect = do
    nodesMap    <- view NodeEditor.nodes <$> Global.getNodeEditor
    newNodesMap <- forM nodesMap $ showOrHideSelfPort mayConnect mayPenConnect
    Global.modifyNodeEditor $ NodeEditor.nodes .= newNodesMap

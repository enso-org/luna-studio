module Reactive.Commands.Graph.Connect
    ( batchConnectNodes
    , localConnectNodes
    ) where


import           Utils.PreludePlus

import           Empire.API.Data.Connection (ConnectionId)
import qualified Empire.API.Data.Port       as Port
import           Empire.API.Data.PortRef    (InPortRef (..), OutPortRef (..))
import qualified Object.Widget.Connection   as ConnectionModel
import qualified React.Store                as Store
import qualified React.Store.NodeEditor     as NodeEditor
import qualified Reactive.Commands.Batch    as BatchCmd
import           Reactive.Commands.Command  (Command)
import qualified Reactive.State.Global      as Global
import qualified Reactive.State.Graph       as Graph
import           UI.Instances               ()



withArrow :: Getter InPortRef Bool
withArrow = to $ \ref -> case ref of
    InPortRef _ Port.Self -> False
    _                     -> True

localConnectNodes :: OutPortRef -> InPortRef -> Command Global.State ConnectionId
localConnectNodes src dst = do
    prevConn <- preuse $ Global.graph . Graph.connectionsMap . ix dst
    connectionId <- zoom Global.graph $ Graph.addConnection src dst
    let newConnection = not $ isJust prevConn
    when newConnection $ do
        Global.withNodeEditor $ Store.modifyM_ $ do
            let connection = ConnectionModel.Connection connectionId True def def (dst ^. withArrow) def def
            connectionRef <- lift $ Store.create connection
            NodeEditor.connections . at dst ?= connectionRef
    return connectionId

batchConnectNodes :: OutPortRef -> InPortRef -> Command Global.State ()
batchConnectNodes src dst = BatchCmd.connectNodes src dst

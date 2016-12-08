module Reactive.Commands.Node.NodeMeta
    ( updateNodesMeta
    , modifyNodeMeta
    ) where

import           Utils.PreludePlus
import           Utils.Vector

import           Empire.API.Data.Node      (NodeId)
import qualified Empire.API.Data.Node      as Node
import           Empire.API.Data.NodeMeta  (NodeMeta (..))
import qualified Empire.API.Data.NodeMeta  as NodeMeta

import qualified Object.Widget.Node        as NodeModel
import qualified React.Store               as Store

import qualified Reactive.Commands.Batch   as BatchCmd
import           Reactive.Commands.Command (Command)
import           Reactive.Commands.Graph   (updateConnectionsForNodes)
import qualified Reactive.State.Global     as Global
import qualified Reactive.State.Graph      as Graph



updateNodeMeta' :: NodeId -> NodeMeta -> Command Global.State ()
updateNodeMeta' nodeId meta = do
    Global.graph . Graph.nodesMap . ix nodeId . Node.nodeMeta .= meta
    nodeRef <- Global.getNode nodeId
    withJust nodeRef $ Store.modifyM_ $ do
        NodeModel.visualizationsEnabled .= meta ^. NodeMeta.displayResult
        NodeModel.position .= fromTuple (meta ^. NodeMeta.position)

-- updateNodeMeta :: NodeId -> NodeMeta -> Command Global.State ()
-- updateNodeMeta nodeId meta = do
--     updateNodeMeta' nodeId meta
--     updateConnectionsForNodes [nodeId]

updateNodesMeta :: [(NodeId, NodeMeta)] -> Command Global.State ()
updateNodesMeta updates = do
    mapM_ (uncurry updateNodeMeta') updates
    updateConnectionsForNodes $ fst <$> updates

modifyNodeMeta :: NodeId -> (NodeMeta -> NodeMeta) -> Command Global.State ()
modifyNodeMeta nid setter = do
    mayOldMeta <- preuse $ Global.graph . Graph.nodesMap . ix nid . Node.nodeMeta
    withJust mayOldMeta $ \oldMeta -> do
        let newMeta = setter oldMeta
        BatchCmd.updateNodeMeta [(nid, newMeta)]

module Reactive.Commands.Node.NodeMeta
    ( updateNodesMeta
    , modifyNodeMeta
    ) where

import           Luna.Studio.Prelude
import           Luna.Studio.Data.Vector

import           Empire.API.Data.Node      (NodeId)
import qualified Empire.API.Data.Node      as Node
import           Empire.API.Data.NodeMeta  (NodeMeta (..))
import qualified Empire.API.Data.NodeMeta  as NodeMeta

import qualified Object.Widget.Node        as NodeModel
import qualified React.Store               as Store

import qualified Reactive.Commands.Batch   as BatchCmd
import           Reactive.Commands.Command (Command)
import qualified Reactive.State.Global     as Global
import qualified Reactive.State.Graph      as Graph



updateNodeMeta' :: NodeId -> NodeMeta -> Command Global.State ()
updateNodeMeta' nodeId meta = do
    Global.graph . Graph.nodesMap . ix nodeId . Node.nodeMeta .= meta
    nodeRef <- Global.getNode nodeId
    withJust nodeRef $ Store.modifyM_ $ do
        NodeModel.visualizationsEnabled .= meta ^. NodeMeta.displayResult
        NodeModel.position .= fromTuple (meta ^. NodeMeta.position)

updateNodeMeta :: NodeId -> NodeMeta -> Command Global.State ()
updateNodeMeta nodeId meta = do
    updateNodeMeta' nodeId meta
    -- TODO[react]: Find out if we need this
    -- updateConnectionsForNodes [nodeId]

updateNodesMeta :: [(NodeId, NodeMeta)] -> Command Global.State ()
updateNodesMeta updates = do
    mapM_ (uncurry updateNodeMeta') updates
    -- TODO[react]: Find out if we need this
    -- updateConnectionsForNodes $ fst <$> updates

modifyNodeMeta :: NodeId -> (NodeMeta -> NodeMeta) -> Command Global.State ()
modifyNodeMeta nid setter = do
    mayOldMeta <- preuse $ Global.graph . Graph.nodesMap . ix nid . Node.nodeMeta
    withJust mayOldMeta $ \oldMeta -> do
        let newMeta = setter oldMeta
        BatchCmd.updateNodeMeta [(nid, newMeta)]

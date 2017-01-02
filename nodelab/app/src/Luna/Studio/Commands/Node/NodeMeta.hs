module Luna.Studio.Commands.Node.NodeMeta
    ( updateNodesMeta
    , modifyNodeMeta
    ) where

import           Luna.Studio.Data.Vector
import           Luna.Studio.Prelude

import           Empire.API.Data.Node         (NodeId)
import qualified Empire.API.Data.Node         as Node
import           Empire.API.Data.NodeMeta     (NodeMeta (..))
import qualified Empire.API.Data.NodeMeta     as NodeMeta

import qualified Luna.Studio.React.Model.Node as NodeModel
import qualified Luna.Studio.React.Store      as Store

import qualified Luna.Studio.Commands.Batch   as BatchCmd
import           Luna.Studio.Commands.Command (Command)
import qualified Luna.Studio.State.Global     as Global
import qualified Luna.Studio.State.Graph      as Graph



updateNodeMeta' :: NodeId -> NodeMeta -> Command Global.State ()
updateNodeMeta' nodeId meta = do
    Global.graph . Graph.nodesMap . ix nodeId . Node.nodeMeta .= meta
    nodeRef <- Global.getNode nodeId
    withJust nodeRef $ Store.modifyM_ $ do
        NodeModel.visualizationsEnabled .= meta ^. NodeMeta.displayResult
        NodeModel.position .= Position (fromTuple $ meta ^. NodeMeta.position)

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
